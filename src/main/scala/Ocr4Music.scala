import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D
import com.twitter.json.Json
import scala.io.Source

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
}

case class LabeledPoint (
  val label:String,
  val x:Int,
  val y:Int
) {}

case class Annotation (
  val caseName:String,
  val points:List[LabeledPoint],
  val notes:List[List[Int]]
) {}

case class Performance (
  val correctNotes:List[(Int,TemplateMatch)],
  val spuriousNotes:List[(Int,TemplateMatch)],
  val missingNotes:List[(Int,Int)]
) {
  def numCorrect() = { correctNotes.size }
  def numSpurious() = { spuriousNotes.size }
  def numMissing() = { missingNotes.size }
  def precision() = { numCorrect.floatValue / (numCorrect + numSpurious) }
  def recall() = { numCorrect.floatValue / (numCorrect + numMissing) }
}

case class ParameterSearch (
  val min:Float,
  val max:Float,
  val step:Float
) {}

case class QuadraticParameterSearch (
  val a:ParameterSearch,
  val b:ParameterSearch,
  val c:ParameterSearch
) {}

case class Metrics (
  val w:Float, // width
  val h:Float, // height
  val a:Float, // x^2 term
  val b:Float, // x term
  val c:Float,  // constant term
  // The amount of separation between each staff line is not constant.
  // It's computed with the linear function:
  //   b = bOverCSlope*c + bIntercept
  val cSpacing:Float,
  val bSpacing:Float
) {}

case class TemplateMatch (
  val x:Int,
  val y:Int,
  val w:Int,
  val h:Int,
  val blackMatch:Int,
  val whiteMatch:Int,
  val blackMatchX:Int,
  val blackMatchY:Int,
  val slope:Float,
  val label:String,
  val staffY:Int
) {}

object Ocr4Music {
  // returns image with just notes, just notes grayscale, and without notes
  def separateNotes(input:GrayImage) = {
    val whiteBackground = input.brighten(130)
    val binaryNonStaff = whiteBackground.blurVertically1.binarize(200)
    val augmentedBinaryNonStaff = binaryNonStaff.blurVertically4.binarize(254)
    val partiallyErased = whiteBackground.addWithCeiling(
      augmentedBinaryNonStaff.inverse)
    (binaryNonStaff, whiteBackground, partiallyErased, augmentedBinaryNonStaff)
  }

  def estimateMetrics(input:GrayImage, caseName:String) : Metrics = {
    val aspectRatio = input.h / input.w.floatValue
    val params = QuadraticParameterSearch(
      ParameterSearch(-0.001f, 0.001f, 0.0001f), // A
      //ParameterSearch(-0.00000f, 0.00005f, 0.1f), // A, to disable curvature
      ParameterSearch(-aspectRatio * 0.6f,
                       aspectRatio * 0.6f, aspectRatio / 100.0f), // B
      ParameterSearch(-input.h.floatValue / 2,
                       input.h.floatValue / 2, 0.25f)) // C

    val numASteps =
      Math.ceil((params.a.max - params.a.min) / params.a.step).intValue + 1
    val numBSteps =
      Math.ceil((params.b.max - params.b.min) / params.b.step).intValue + 1
    val numCSteps =
      Math.ceil((params.c.max - params.c.min) / params.c.step).intValue + 1
    val hough = new Array[Int](numASteps * numBSteps * numCSteps)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = 255 - input(x, y)
        val xCentered = x - input.w / 2
        val yCentered = y - input.h / 2

        var a = params.a.min
        var aSteps = 0
        while (a <= params.a.max) {
          var b = params.b.min
          var bSteps = 0
          while (b <= params.b.max) {
            val cSolved =
              yCentered - (a * xCentered * xCentered) - (b * xCentered)
            val cSteps = ((cSolved - params.c.min) / params.c.step).intValue
            if (cSteps >= 4 && cSteps < numCSteps - 4) {
              val i = aSteps * numBSteps * numCSteps +
                      bSteps * numCSteps + cSteps
              (-3 to 3).foreach { j =>
                hough(i + j) += (v * (4 - Math.abs(j)))
              }
            }
            b += params.b.step
            bSteps += 1
          }
          a += params.a.step
          aSteps += 1
        }
      }
    }

    var max = 0
    var argmax = 0
    (0 until hough.length).foreach { i =>
      if (hough(i) > max) {
        max = hough(i)
        argmax = i
      }
    }
    val bestASteps = argmax / numBSteps / numCSteps
    val bestBSteps = (argmax - (bestASteps * numBSteps * numCSteps)) / numCSteps
    val bestCSteps = argmax % numCSteps
    val bestA = bestASteps * params.a.step + params.a.min
    val bestB = bestBSteps * params.b.step + params.b.min
    val bestC = bestCSteps * params.c.step + params.c.min

    // we have to extract the hough2 for the bestASteps plane
    val hough2 = new GrayImage(numCSteps, numBSteps)
    (0 until hough2.w).foreach { cSteps =>
      (0 until hough2.h).foreach { bSteps =>
        val i = bestASteps * numBSteps * numCSteps + bSteps * numCSteps + cSteps
        hough2(cSteps, bSteps) = hough(i)
      }
    }

    // Find brightest point
    var brightestCSteps = 0
    var brightestBSteps = 0
    var maxBrightness = 0
    (0 until hough2.h).foreach { bSteps =>
      (0 until hough2.w).foreach { cSteps =>
        val v = hough2(cSteps, bSteps)
        if (v > maxBrightness) {
          maxBrightness = v
          brightestCSteps = cSteps
          brightestBSteps = bSteps
        }
      }
    }

    // Look for the brightest sequence of 5 points all in a line,
    // one of them being the brightest point found earlier
    var maxSum = -999999f
    var bestBOverCSlope = 0.0f
    var bestBrightestIsPointN = 0
    var bestStaffSeparationInCAxis = 0.0f
    var staffSeparationInCAxis = 4.0f / params.c.step
    while (staffSeparationInCAxis <= 20.0f / params.c.step) {
      // 2.0f allows the b/c slope to trace over top of the lines corresponding
      // with lines all emanating from one point.  On the other hand, a too
      // small constant doesn't the distance between staffs to change much
      // between the left side of the image and the right side.
      val maxBOverCSlope = 1.9f * params.c.step / params.b.step / input.w
      val minBOverCSlope = -maxBOverCSlope
      var bOverCSlope = minBOverCSlope
      while (bOverCSlope <= maxBOverCSlope) {
        (-2 to 2).foreach { brightestIsPointN =>
          var leftmost = -2 - brightestIsPointN
          var rightmost = 2 - brightestIsPointN
          val c0 =
            (brightestCSteps + staffSeparationInCAxis * leftmost).intValue
          val c1 =
            (brightestCSteps + staffSeparationInCAxis * rightmost).intValue
          val b0 =
            brightestBSteps + staffSeparationInCAxis * bOverCSlope * leftmost
          val b1 =
            brightestBSteps + staffSeparationInCAxis * bOverCSlope * rightmost
          var bSteps = b0
          var sum = 1.0f
          (leftmost to rightmost).foreach { n =>
            val cSteps = Math.round(brightestCSteps +
              staffSeparationInCAxis * n).intValue
            val bSteps =
              Math.round(brightestBSteps +
              staffSeparationInCAxis * bOverCSlope * n).intValue
            val v = hough2(cSteps, bSteps)
            sum += v
          }
          if (sum > maxSum) {
            maxSum = sum
            bestBOverCSlope = bOverCSlope
            bestBrightestIsPointN = brightestIsPointN
            bestStaffSeparationInCAxis = staffSeparationInCAxis
          }
        }
        bOverCSlope += 0.01f
      }
      staffSeparationInCAxis += 0.01f
    }

    // Draw 5x5 square on brightest point
    (-2 to 2).foreach { bNeighbor =>
      (-2 to 2).foreach { cNeighbor =>
        hough2(brightestCSteps + cNeighbor, brightestBSteps + bNeighbor) = 5000
      }
    }

    // Draw 3x3 squares around 5 points, with black dots in the center
    (0 until 5).foreach { i =>
      val offset = i - 2 - bestBrightestIsPointN
      val cCenter = Math.round(brightestCSteps +
        bestStaffSeparationInCAxis * offset).intValue
      val bCenter = Math.round(brightestBSteps +
        bestStaffSeparationInCAxis * bestBOverCSlope * offset).intValue
      (-1 to 1).foreach { bNeighbor =>
        (-1 to 1).foreach { cNeighbor =>
          hough2(cCenter + cNeighbor, bCenter + bNeighbor) = 5000
        }
      }
      hough2(cCenter, bCenter) = 0
    }

    // Scale down the hough2 image so it's between 0-255
    val max2 = hough2.data.max 
    (0 until hough2.w).foreach { c =>
      (0 until hough2.h).foreach { b =>
        hough2(c, b) = (hough2(c, b) * (250.0 / max2)).intValue
      }
    }
    hough2.saveTo(new File("demos/hough.%s.png".format(caseName)))

    val centerCSteps =
      brightestCSteps + bestStaffSeparationInCAxis * -bestBrightestIsPointN
    val centerBSteps =
      brightestBSteps + bestStaffSeparationInCAxis * bestBOverCSlope *
      -bestBrightestIsPointN
    val cSpacingSteps = bestStaffSeparationInCAxis
    val bSpacingSteps = bestStaffSeparationInCAxis * bestBOverCSlope

    // convert from step (array index) coordinates to usable coordinates
    val centerC = centerCSteps * params.c.step + params.c.min
    val centerB = centerBSteps * params.b.step + params.b.min
    val cSpacing = cSpacingSteps * params.c.step
    val bSpacing = bSpacingSteps * params.b.step

    Metrics(input.w, input.h, bestA, centerB, centerC, cSpacing, bSpacing)
  }

  def demoNotes(
      noteGroups:List[List[TemplateMatch]], input:ColorImage, caseName:String) {
    val staffSeparation = 6
    val xSeparation = 18
    val darkYellow = (128, 128, 0)
    val brightYellow = (255, 255, 0)
    val staffHeight = 100
    val image = new ColorImage(input.w, staffHeight + input.h)

    // draw notes
    var staffX = -1
    noteGroups.foreach { noteGroup =>
      staffX += 1
    noteGroup.foreach { note =>
      val staffY = note.staffY
      val centerY = (staffHeight / 2) + (staffY * staffSeparation / 2).intValue
      (-8 to 8).foreach { x =>
        (-8 to 8).foreach { y =>
          if ((x * x) + 2 * (y * y) < 20) {
            image((staffX + 1) * xSeparation + x, centerY + y) =
              brightYellow
          }
        }
      }

      def drawLedgerLine(staffY:Int) {
        val ledgerLineY = ((staffY / 2).intValue * 2 * staffSeparation / 2) +
          (staffHeight / 2)
        (-8 to 8).foreach { x =>
          image((staffX + 1) * xSeparation + x, ledgerLineY) = darkYellow
        }
      }

      // draw ledger lines
      var staffYTemp = staffY
      while (staffYTemp >= 6) {
        drawLedgerLine(staffYTemp)
        staffYTemp -= 2
      }
      while (staffYTemp <= -6) {
        drawLedgerLine(staffYTemp)
        staffYTemp += 2
      }

      // draw staff (top-most so line vs. space notes are more obvious)
      (-4 to 4 by 2).foreach { staffYTemp =>
        (0 until image.w).foreach { x =>
          image(x, (staffHeight / 2) + (staffYTemp * staffSeparation / 2)) =
            darkYellow
        }
      }
    } // next note in noteGroup
    } // next noteGroup

    // copy input
    (0 to input.h).foreach { y =>
      (0 to input.w).foreach { x =>
        image(x, y + staffHeight) = input(x, y)
      }
    }

    image.saveTo(new File("demos/notes2.%s.png".format(caseName)))
  }

  def demoPointGroups(groups:List[Set[TemplateMatch]], input:GrayImage,
      caseName:String) {
    val demo = input.toColorImage
    groups.foreach { group =>
      val x0 = group.foldLeft(9999) { (accum, point) =>
        accum min (point.x - (point.w+1)/2)
      }
      val x1 = group.foldLeft(-9999) { (accum, point) =>
        accum max (point.x + (point.w+1)/2)
      }
      val y0 = group.foldLeft(9999) { (accum, point) =>
        accum min (point.y - (point.h+1)/2)
      }
      val y1 = group.foldLeft(-9999) { (accum, point) =>
        accum max (point.y + (point.h+1)/2)
      }

      (x0 to x1).foreach { x =>
        demo(x, y0) = (255, 0, 0)
        demo(x, y1) = (255, 0, 0)
      }
      (y0 to y1).foreach { y =>
        demo(x0, y) = (255, 0, 0)
        demo(x1, y) = (255, 0, 0)
      }
    }
    demo.saveTo(new File("demos/point_groups.%s.png".format(caseName)))
  }

  def groupTemplateMatches(matches:List[TemplateMatch]) = {
    val matchesSorted = matches.sort { (match1, match2) =>
      match1.x < match2.x
    }

    var noteGroups:List[List[TemplateMatch]] = Nil
    var currentNoteGroup:List[TemplateMatch] = Nil
    var lastNoteX = -1
    matchesSorted.foreach { _match =>
      if (Math.abs(_match.x - lastNoteX) >= 15 && currentNoteGroup.size > 0) {
        noteGroups = currentNoteGroup :: noteGroups
        currentNoteGroup = Nil
      }

      currentNoteGroup = _match :: currentNoteGroup

      lastNoteX = _match.x
    }
    if (currentNoteGroup.size > 0)
      noteGroups = currentNoteGroup :: noteGroups

    noteGroups.reverse
  }

  def loadAnnotationJson(caseName:String) : Annotation = {
    val filename = "input/%s.json".format(caseName)
    val annotationsString:String =
      scala.io.Source.fromFile(filename).mkString
    val annotationJson:Map[String,Any] = 
      Json.parse(annotationsString).asInstanceOf[Map[String,Any]]
    val points =
      annotationJson("points").asInstanceOf[List[Map[String,Any]]].map {
        pointJson => LabeledPoint(
          pointJson("type").asInstanceOf[String],
          pointJson("x").asInstanceOf[Int],
          pointJson("y").asInstanceOf[Int]
        )
      }
    val annotatedNotes = annotationJson("notes").asInstanceOf[List[List[Int]]]

    Annotation(caseName, points, annotatedNotes)
  }

  // Levenshtein minimum-edit-distance to determine best alignment
  // between detected note groups and ground truth note groups.
  // This ensures that a spurious or missed note only causes one
  // error instead of throwing off all the notes to the right.
  def calcPerformance(
      estimatedNotes:List[List[TemplateMatch]], annotated:List[List[Int]]) = {
    val estimated = estimatedNotes.map { _.map { _.staffY } }
    var w = estimated.size
    var h = annotated.size
    var matrix = new Array[Array[Int]](h + 1)
    var backPointer = new Array[Array[(Int,Int)]](h + 1)
    (0 to h).foreach { y =>
      matrix(y) = new Array[Int](w + 1)
      backPointer(y) = new Array[(Int,Int)](w + 1)
    }

    case class Possibility (val direction:(Int,Int), val score:Int) {}
    (0 to h).foreach { y =>
      (0 to w).foreach { x =>
        var possibilities:List[Possibility] = Nil
        if (x > 0) {
          // If this noteGroup is totally spurious
          val scoreIncrease = estimated.size
          val score = matrix(y)(x - 1) + scoreIncrease
          possibilities = Possibility((-1, 0), score) :: possibilities
        }
        if (y > 0) {
          // If this noteGroup is totally missing from prediction
          val scoreIncrease = annotated.size
          val score = matrix(y - 1)(x) + scoreIncrease
          possibilities = Possibility((0, -1), score) :: possibilities
        }
        if (y > 0 && x > 0) {
          // If this predicted noteGroup corresponds with the next
          // annotated noteGroup
          var scoreIncrease = 0
          estimated(x - 1).foreach { i =>
            if (!(annotated(y - 1).contains(i)))
              scoreIncrease += 1
          }
          annotated(y - 1).foreach { i =>
            if (!(estimated(x - 1).contains(i)))
              scoreIncrease += 1
          }

          val score = matrix(y - 1)(x - 1) + scoreIncrease
          possibilities = Possibility((-1, -1), score) :: possibilities
        }

        if (x == 0 && y == 0) {
          matrix(y)(x) = 0
          backPointer(y)(x) = (0, 0)
        } else {
          var minScore = 99999
          var bestDirection = (0, 0)
          possibilities.foreach { possibility =>
            if (possibility.score < minScore) {
              minScore = possibility.score
              bestDirection = possibility.direction
            }
          }
          matrix(y)(x) = minScore
          backPointer(y)(x) = bestDirection
        }
      }
    }

    case class PairedNoteGroup (
      val staffX:Int,
      val estimatedNotes:List[TemplateMatch],
      val annotated:List[Int]
    ) {}
    var pairedNoteGroups:List[PairedNoteGroup] = Nil
    var x = w
    var y = h
    while (x > 0 || y > 0) {
      backPointer(y)(x) match {
        case (-1, 0) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimatedNotes(x - 1), List()) ::
            pairedNoteGroups
          x -= 1
        case (0, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, List(), annotated(y - 1)) ::
            pairedNoteGroups
          y -= 1
        case (-1, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimatedNotes(x - 1), annotated(y - 1)) ::
            pairedNoteGroups
          x -= 1
          y -= 1
      }
    }
    pairedNoteGroups = pairedNoteGroups.reverse

    var correctNotes:List[(Int,TemplateMatch)] = Nil // staffX, note
    var spuriousNotes:List[(Int,TemplateMatch)] = Nil // staffX, note
    var missingNotes:List[(Int,Int)] = Nil // staffX, staffY
    pairedNoteGroups.foreach { pair =>
      val PairedNoteGroup(staffX, estimatedNotes, annotated) = pair
      estimatedNotes.foreach { note =>
        if (!(annotated.contains(note.staffY)))
          spuriousNotes = (staffX, note) :: spuriousNotes
        else
          correctNotes = (staffX, note) :: correctNotes
      }
      val estimatedStaffYs = estimatedNotes.map { _.staffY }
      annotated.foreach { i =>
        if (!(estimatedStaffYs.contains(i)))
          missingNotes = (staffX, i) :: missingNotes
      }
    }

    Performance(correctNotes, spuriousNotes, missingNotes)
  }

  def demoStaffLines(excerpt:GrayImage, metrics:Metrics,
      yCorrection:Array[Float], caseName:String) {
    val demo = excerpt.resize(excerpt.w * 4, excerpt.h * 4, 0)
    // -8 to 8 instead of -4 to 4 because we want to include ledger lines
    (-8 to 8 by 2).foreach { staffY =>
      (0 until excerpt.w).foreach { xUncentered =>
        val x = xUncentered - (excerpt.w / 2)
        val a = metrics.a
        val b = metrics.b + (staffY / 2 * metrics.bSpacing)
        val c = metrics.c + (staffY / 2 * metrics.cSpacing)
        val y = (a * x * x + b * x + c) + yCorrection(xUncentered) +
          (excerpt.h / 2)
        val color = (if (Math.abs(staffY) <= 4) 255 else 127)
        if (y >= 0 && Math.round(y * 4).intValue < demo.h)
          demo(xUncentered * 4, Math.round(y * 4).intValue) = color

        /*val b = metrics.staffYToB(staffY + 2)
        val c = metrics.staffYToC(staffY + 2)
        val y = (a * x * x + b * x + c) + (excerpt.h / 2)
        if (y >= 0 && Math.round(y * 4).intValue < demo.h)
          demo(xUncentered * 4, Math.round(y * 4).intValue) = 255*/
      }
    }
    demo.saveTo(new File("demos/staff_lines.%s.png".format(caseName)))
  }

  def determineYCorrection(
      partiallyErased:GrayImage, augmentedBinaryNonStaff:GrayImage,
      metrics:Metrics, caseName:String) = {
    val justStaff = new GrayImage(partiallyErased.w, partiallyErased.h)

    // white background
    (0 until partiallyErased.h).foreach { y =>
      (0 until partiallyErased.w).foreach { x =>
        justStaff(x, y) = 255
      }
    }

    val yCorrection = new Array[Float](partiallyErased.w)
    (0 until partiallyErased.w).foreach { xUncentered =>
      var sumDarkestYNeighbor = 0
      var encounteredNote = false
      var numDarkestYNeighbors = 0
      (-4 to 4 by 2).foreach { staffY =>
        val x = xUncentered - (partiallyErased.w / 2)
        val a = metrics.a
        val b = metrics.b + (staffY / 2 * metrics.bSpacing)
        val c = metrics.c + (staffY / 2 * metrics.cSpacing)
        val y = Math.round(
          (a * x * x + b * x + c) + (partiallyErased.h / 2)).intValue
        val staffSeparation =
          ((x * metrics.bSpacing) + metrics.cSpacing).intValue
        if (y >= staffSeparation/2 && y < justStaff.h - staffSeparation/2) {
          var darkestYNeighbor = 0
          var maxDarkness = 0
          (-staffSeparation/2 to staffSeparation/2).foreach { yNeighbor =>
            if (augmentedBinaryNonStaff(xUncentered, y + yNeighbor) != 0) {
              val v = 255 - partiallyErased(xUncentered, y + yNeighbor)
              justStaff(xUncentered, y + yNeighbor) = 255 - v
              if (v > maxDarkness) {
                maxDarkness = v
                darkestYNeighbor = yNeighbor
              }
            }
            else
              encounteredNote = true
          }
          if (!encounteredNote) {
            sumDarkestYNeighbor += darkestYNeighbor
            numDarkestYNeighbors += 1
          }
        }
      }

      if (numDarkestYNeighbors == 5) {
        val averageY = sumDarkestYNeighbor / 5.0f
        yCorrection(xUncentered) = averageY
      }
      else
        yCorrection(xUncentered) = 0.0f
    }
    //justStaff.saveTo(new File("line_strength.png".format(caseNum)))

    val smoothedYCorrection = new Array[Float](partiallyErased.w)
    (0 until partiallyErased.w).foreach { outerX =>
      val x0 = outerX - 20 max 0
      val x1 = outerX + 20 min partiallyErased.w
      var values:List[Float] = Nil
      (x0 until x1).foreach { innerX =>
        if (yCorrection(innerX) != 0.0f) {
          values = yCorrection(innerX) :: values
        }
      }
      val median =
        if (values.length > 0)
          values.sort { (v1, v2) => v1 < v2 }(values.length / 2)
        else
          0.0f
      smoothedYCorrection(outerX) = median
    }

    smoothedYCorrection
  }

  def scaleTemplate(template:GrayImage, templateW:Int, templateH:Int) = {
    val templateScaled = new GrayImage(templateW, templateH)
    (0 until templateH).foreach { templateScaledY =>
      (0 until templateW).foreach { templateScaledX =>
        val y0 = templateScaledY * template.h / templateH
        val y1 =
          ((templateScaledY + 1) * template.h / templateH) min template.h
        val x0 = templateScaledX * template.w / templateW
        val x1 =
          ((templateScaledX + 1) * template.w / templateW) min template.w
        if (y1 > y0 && x1 > x0) {
          var sum = 0
          (y0 until y1).foreach { templateY =>
            (x0 until x1).foreach { templateX =>
              sum += template(templateX, templateY)
            }
          }
            val mean = sum / (y1 - y0) / (x1 - x0)
          templateScaled(templateScaledX, templateScaledY) = mean
        }
        else
          templateScaled(templateScaledX, templateScaledY) = 255
      }
    }
    templateScaled
  }

  def findBestMatchAroundPoint(templateSum:GrayImage, inputAdjusted:GrayImage,
      expectedX:Int, expectedY:Int, metrics:Metrics, label:String,
      staffY:Int) = {

    val (minX, maxX) = label match {
      case "L" | "S" | "Sa" | "Sb" | "2" =>
        (expectedX - 2, expectedX + 2)
      case "#" =>
        (expectedX - 3, expectedX + 3)
      case "b" | "N" =>
        (expectedX - 2, expectedX + 2)
    }
    val (minY, maxY) = label match {
      case "L" | "S" | "Sa" | "Sb" =>
        (expectedY - 2, expectedY + 2)
      case "2" =>
        (expectedY - 2, expectedY + 4)
      case "#" =>
        (expectedY + 0, expectedY + 10)
      case "b" | "N" =>
        (expectedY + 2, expectedY + 2)
    }

    findBestMatch(templateSum, inputAdjusted,
      (minX, maxX), (minY, maxY), metrics, label, staffY)
  }

  def findBestMatch(templateSum:GrayImage, inputAdjusted:GrayImage,
      minXmaxX:(Int,Int), minYmaxY:(Int,Int), 
      metrics:Metrics, label:String, staffY:Int) = {
    val (minX, maxX) = minXmaxX
    val (minY, maxY) = minYmaxY

    // Differentiate axx + bx + c to get 2ax + b as slope
    val expectedX = (minX + maxX) / 2
    val xCentered = expectedX - metrics.w / 2
    val slope = (2.0f * metrics.a * xCentered) + metrics.b
    val staffSeparation =
      ((xCentered * metrics.bSpacing) + metrics.cSpacing).intValue

    val (minW, maxW) = label match {
      case "L" | "S" | "Sa" | "Sb" | "2" =>
        (staffSeparation, staffSeparation * 3)
      case "#" | "b" | "N" =>
        (staffSeparation * 3/4, staffSeparation * 3/2)
    }
    val (minH, maxH) = label match {
      case "L" | "S" | "Sa" | "Sb" | "2" =>
        (staffSeparation, staffSeparation * 3/2)
      case "#" =>
        (staffSeparation * 5/2, staffSeparation * 3)
      case "b" | "N" =>
        (staffSeparation * 2, staffSeparation * 4)
    }

    var bestTemplateW = 0
    var bestTemplateH = 0
    var bestInputCenterX = 0
    var bestInputCenterY = 0
    var bestBlackMatch = 0
    var bestWhiteMatch = 0
    var bestBlackMatchX = 0
    var bestBlackMatchY = 0
    var maxCombinedMatch = 0
    (minW to maxW).foreach { templateW =>
    (minH to maxH).foreach { templateH =>
      (minY to maxY).foreach { inputCenterY =>
        (minX to maxX).foreach { inputCenterX =>
          var sumBlackMatch = 0
          var sumWhiteMatch = 0
          var sumBlackMatchX = 0
          var sumBlackMatchY = 0
          (0 until templateH).foreach { templateScaledY =>
            (0 until templateW).foreach { templateScaledX =>
              val yAdjustment = Math.round(
                (templateScaledX - templateW / 2) * slope).intValue
              val inputV = inputAdjusted(
                inputCenterX + templateScaledX - templateW / 2,
                inputCenterY + yAdjustment +
                  templateScaledY - templateH / 2)

              val templateFullX0 =
                templateScaledX * templateSum.w / templateW - 1
              val templateFullX1 =
                (templateScaledX + 1) * templateSum.w / templateW - 1
              val templateFullY0 =
                templateScaledY * templateSum.h / templateH - 1
              val templateFullY1 =
                (templateScaledY + 1) * templateSum.h / templateH - 1
              val templateVSum =
                templateSum(templateFullX1, templateFullY1) -
                templateSum(templateFullX0, templateFullY1) -
                templateSum(templateFullX1, templateFullY0) +
                templateSum(templateFullX0, templateFullY0)
              val templateV = templateVSum /
                (templateFullX1 - templateFullX0) /
                (templateFullY1 - templateFullY0)

              sumBlackMatch += (255 - inputV) * (255 - templateV)
              sumWhiteMatch += inputV * templateV
              sumBlackMatchX += (255 - inputV) * (255 - templateV) *
                (templateScaledX - templateW / 2)
              sumBlackMatchY += (255 - inputV) * (255 - templateV) *
                (templateScaledY - templateH / 2)
            }
          }
          val meanBlackMatch = sumBlackMatch /
            (255 * templateW * templateH)
          val meanWhiteMatch = sumWhiteMatch /
            (255 * templateW * templateH)
          val meanBlackMatchX = Math.abs(sumBlackMatchX /
            (255 * templateW * templateH * templateW))
          val meanBlackMatchY = Math.abs(sumBlackMatchY /
            (255 * templateW * templateH * templateH))

          val combinedMatch = meanBlackMatch + meanWhiteMatch
          if (combinedMatch > maxCombinedMatch) {
            maxCombinedMatch = combinedMatch
            bestInputCenterX = inputCenterX
            bestInputCenterY = inputCenterY
            bestTemplateW = templateW
            bestTemplateH = templateH
            bestBlackMatch = meanBlackMatch
            bestWhiteMatch = meanWhiteMatch
            bestBlackMatchX = meanBlackMatchX
            bestBlackMatchY = meanBlackMatchY
          }
        }
      }
    }
    }
    TemplateMatch(
      bestInputCenterX, bestInputCenterY, bestTemplateW, bestTemplateH,
      bestBlackMatch, bestWhiteMatch, bestBlackMatchX, bestBlackMatchY,
      slope, label, staffY)
  }

  def drawTemplateMatch(_match:TemplateMatch, output:ColorImage,
      template:GrayImage) {
    val blackest = 50
    val whitest = 100
    val templateScaled = scaleTemplate(template, _match.w, _match.h)
    (0 until templateScaled.h).foreach { templateScaledY =>
      (0 until templateScaled.w).foreach { templateScaledX =>
        val yAdjustment = Math.round((templateScaledX - templateScaled.w / 2) *
          _match.slope).intValue
        val inputV = output(
          _match.x + templateScaledX - templateScaled.w / 2,
          _match.y + yAdjustment +
            templateScaledY - templateScaled.h / 2)._1
        val v2 = (inputV - blackest) * 255 / (whitest - blackest)
        val inputAdjustedV = (if (v2 < 0) 0 else if (v2 > 255) 255 else v2)
        val templateV = templateScaled(templateScaledX, templateScaledY)
        val blackMatch = (255 - inputAdjustedV) * (255 - templateV)
        val whiteMatch = inputAdjustedV * templateV
        val demoX = (_match.x + templateScaledX - templateScaled.w / 2)
        val demoY = (_match.y + yAdjustment +
          templateScaledY - templateScaled.h / 2)
        val (r, g, b) = output(demoX, demoY)
        output(demoX, demoY) = (blackMatch / 255, 0, whiteMatch / 255)
      }
    }
  }

  def dedupe(points:List[TemplateMatch]) : List[TemplateMatch] = {
    if (points.isEmpty)
      points
    else
      points.head :: dedupe(
        for (x <- points.tail if x.staffY != points.head.staffY) yield x)
  }

  def sumTemplate(input:GrayImage) : GrayImage = {
    val sum = new GrayImage(input.w, input.h)

    (0 until sum.h).foreach { y =>
      (0 until sum.w).foreach { x =>
        sum(x, y) = input(x, y)
      }
    }

    (1 until sum.h).foreach { y =>
      (0 until sum.w).foreach { x =>
        sum(x, y) = sum(x, y) + sum(x, y - 1)
      }
    }

    (0 until sum.h).foreach { y =>
      (1 until sum.w).foreach { x =>
        sum(x, y) = sum(x, y) + sum(x - 1, y)
      }
    }

    sum
  }

  // returns area of overlap divided by total area of two points
  def overlapAmount(point1:TemplateMatch, point2:TemplateMatch) = {
    val (x10, x11) = (point1.x - point1.w/2.0f, point1.x + point1.w/2.0f)
    val (y10, y11) = (point1.y - point1.h/2.0f, point1.y + point1.h/2.0f)
    val (x20, x21) = (point2.x - point2.w/2.0f, point2.x + point2.w/2.0f)
    val (y20, y21) = (point2.y - point2.h/2.0f, point2.y + point2.h/2.0f)
    val (x30, x31) = (x10 max x20, x11 min x21)
    val (y30, y31) = (y10 max y20, y11 min y21)
    val (overlapW, overlapH) = ((x31 - x30) max 0, (y31 - y30) max 0)
    (overlapW * overlapH).floatValue /
      ((point1.w min point2.w) * (point1.h min point2.h))
  }

  def groupOverlappingPoints(points:List[TemplateMatch]) = {
    var groups:List[Set[TemplateMatch]] = Nil
    points.foreach { point =>
      groups.filter { group =>
        group.exists { overlapAmount(point, _) >= 0.1f }
      } match {
        case Nil =>
          groups = Set(point) :: groups
        case oneGroup :: Nil =>
          groups = (oneGroup + point) :: (groups - oneGroup)
        case manyGroups =>
          val merged = manyGroups.reduceLeft { _ ++ _ } ++ Set(point)
          groups = merged :: groups -- manyGroups
      }
    }
    groups
  }

  def listNonOverlappingAlternatives(points:List[TemplateMatch])
      : Set[Set[TemplateMatch]] = {
    points match {
      case Nil => Set(Set[TemplateMatch]())
      case point :: otherPoints =>
        val overlapping = otherPoints.filter { otherPoint =>
          overlapAmount(point, otherPoint) >= 0.3f
        }
        val ifPointKept = listNonOverlappingAlternatives(
            otherPoints -- overlapping).map { set => set ++ Set(point) }
        if (overlapping.isEmpty)
          ifPointKept
        else
          ifPointKept ++ listNonOverlappingAlternatives(otherPoints)
    }
  }

  def demoAlternatives(overlappingPointGroups:List[Set[TemplateMatch]],
      input:GrayImage, caseName:String) {
    val demo2 = input.toColorImage
    overlappingPointGroups.foreach { group =>
      val alternatives = listNonOverlappingAlternatives(group.toList)
      var i = 0
      alternatives.foreach { group2 =>
        val color = i match {
          case 0 => (255,0,0)
          case 1 => (0,255,0)
          case 2 => (0,0,255)
          case _ => (255,255,255)
        }

        // for example: gray + red drawn lightly = reddish gray
        def drawLightly(x:Int, y:Int) = {
          val (r, g, b) = demo2(x, y)
          demo2(x, y) = ((r + color._1) min 255,
                         (g + color._2) min 255,
                         (b + color._3) min 255)
        }

        group2.foreach { point =>
          val (x0, x1) = (point.x - (point.w+1)/2, point.x + (point.w+1)/2)
          val (y0, y1) = (point.y - (point.h+1)/2, point.y + (point.h+1)/2)
          (x0 to x1).foreach { x =>
            drawLightly(x, y0)
            drawLightly(x, y1)
          }
          (y0 to y1).foreach { y =>
            drawLightly(x0, y)
            drawLightly(x1, y)
          }
        }

        i += 1
      }
    }
    demo2.saveTo(new File("demos/alternatives.%s.png".format(caseName)))
  }

  def doTemplateMatching(caseNames:List[String]) {
    val templateSharp = 
      ColorImage.readFromFile(new File("templates/sharp.png")).toGrayImage
    val templateFlat =
      ColorImage.readFromFile(new File("templates/flat.png")).toGrayImage
    val templateNatural =
      ColorImage.readFromFile(new File("templates/natural.png")).toGrayImage
    val templateBlackHead =
      ColorImage.readFromFile(new File("templates/black_head.png")).toGrayImage
    val templateWhiteHead =
      ColorImage.readFromFile(new File("templates/white_head.png")).toGrayImage

    var globalPerformance = Performance(List(), List(), List())
    caseNames.foreach { caseName =>
      val imageFilename = "input/%s.jpeg".format(caseName)
      val input =
        ColorImage.readFromFile(new File(imageFilename)).toGrayImage
      val annotation = loadAnnotationJson(caseName)

      val (_, _, partiallyErased, augmentedBinaryNonStaff) =
        separateNotes(input)
      val metrics = estimateMetrics(partiallyErased, caseName)
      val yCorrection = determineYCorrection(
        partiallyErased, augmentedBinaryNonStaff, metrics, caseName)
      demoStaffLines(input, metrics, yCorrection, caseName)

      val blackest = 50
      val whitest = 100
      val inputAdjusted = new GrayImage(input.w, input.h)
      (0 until input.h).foreach { y =>
        (0 until input.w).foreach { x =>
          val v = (input(x, y) - blackest) * 255 / (whitest - blackest)
          inputAdjusted(x, y) = (if (v < 0) 0 else if (v > 255) 255 else v)
        }
      }
      inputAdjusted.saveTo(new File(
        "demos/input_adjusted.%s.png".format(caseName)))

      var points:List[TemplateMatch] = Nil
      List("L", "2", "#").foreach { label =>
        val templateSum = label match {
          case "L" => sumTemplate(templateBlackHead)
          case "2" => sumTemplate(templateWhiteHead)
          case "#" => sumTemplate(templateSharp)
        }

        (-8 to 8).foreach { staffY =>
println(("label", label, "staffY", staffY))
          (0 until augmentedBinaryNonStaff.w).foreach { x =>
            val xCentered = x - (input.w / 2)
            val a = metrics.a
            val b = metrics.b + (staffY / 2.0f * metrics.bSpacing)
            val c = metrics.c + (staffY / 2.0f * metrics.cSpacing)
            val y = Math.round((a * xCentered * xCentered + b * xCentered + c) +
              yCorrection(x) + (input.h / 2)).intValue

            // remove the bands at the top and the bottom, which are probably
            // artifacts from the vertical blurring
            if (y > 4 && y < augmentedBinaryNonStaff.h - 5 &&
                augmentedBinaryNonStaff(x, y) == 0) {
              val isLedgerLine = Math.abs(staffY / 2) > 2
              val yRange = if (isLedgerLine) (y - 1, y + 1) else (y, y)
              val newPoint = findBestMatch(templateSum, inputAdjusted,
                  (x, x), yRange, metrics, label, staffY)
              points = newPoint :: points
            }
          }
        }
      }

      val pointsFiltered = points.filter { point1 =>
        var hasBetterNeighbor = false
        points.foreach { point2 =>
          if (Math.abs(point2.x - point1.x) <= 1 &&
              Math.abs(point2.y - point1.y) <= 1 &&
              point2.label == point1.label) {
            val score1 = point1.blackMatch + point1.whiteMatch
            val score2 = point2.blackMatch + point2.whiteMatch
            if (score1 < score2)
              hasBetterNeighbor = true
            else if (score1 == score2 && point1.y < point2.y)
              hasBetterNeighbor = true
            else if (score1 == score2 &&
                point1.y == point2.y && point1.x < point2.x)
              hasBetterNeighbor = true
          }
        }

        val strongEnough = point1.label match {
          case "L" => point1.blackMatch > 98
          case "2" => point1.blackMatch > 60 && point1.whiteMatch > 90
          case "#" => point1.blackMatch > 30 && point1.whiteMatch > 140
        }

        !hasBetterNeighbor && strongEnough
      }

      val overlappingPointGroups = groupOverlappingPoints(pointsFiltered)
      demoAlternatives(overlappingPointGroups, input, caseName)
      demoPointGroups(overlappingPointGroups, input, caseName)

      val culledPointGroups = overlappingPointGroups.map { group =>
        val alternatives = listNonOverlappingAlternatives(group.toList)
        var bestAlternative = alternatives.toList(0)
        var maxScore = 0
        alternatives.foreach { group2 =>
          var score = 0
          group2.foreach { point =>
            score += point.blackMatch * point.w * point.h
          }

          if (score > maxScore) {
            maxScore = score
            bestAlternative = group2
          }
        }
        bestAlternative
      }.foldLeft(List[TemplateMatch]()) { _ ++ _ }

      val groupedPoints = groupTemplateMatches(culledPointGroups)
      val filteredNotes = groupedPoints

      val demo = input.toColorImage
      filteredNotes.foreach { noteGroup =>
        noteGroup.foreach { point =>
          val template = point.label match {
            case "L" => templateBlackHead
            case "2" => templateWhiteHead
            case "#" => templateSharp
          }
          drawTemplateMatch(point, demo, template)
        }
      }
      demo.saveTo(new File("demos/notes.%s.png".format(caseName)))
      demoNotes(filteredNotes, input.toColorImage, caseName)

      val nonAccidentalNotes = filteredNotes.map { _.filter { _.label != "#" } }
      val performance = calcPerformance(nonAccidentalNotes, annotation.notes)
      println("Case %2s: precision: %.3f, recall: %.3f".format(
        annotation.caseName, performance.precision, performance.recall))
      //printf("  correct: %s\n", performance.correctNotes)
      if (performance.spuriousNotes.size > 0)
        printf("  spurious: %s\n", performance.spuriousNotes)
      if (performance.missingNotes.size > 0)
        printf("  missing: %s\n", performance.missingNotes)

      globalPerformance = Performance(
        globalPerformance.correctNotes ++ performance.correctNotes,
        globalPerformance.spuriousNotes ++ performance.spuriousNotes,
        globalPerformance.missingNotes ++ performance.missingNotes)
    }
    println("Total:   precision: %.3f -- recall: %.3f".format(
      globalPerformance.precision, globalPerformance.recall))
  }

  def oneCaseNameOrAllIfEmpty(args:Array[String]) = {
    args.length match {
      case 0 =>
        val filenames = new File("input").listFiles
        val JsonFilename = """(.*)\.json""".r
        filenames.foldLeft(List[String]()) { (accum, filename) =>
          filename.getName match {
            case JsonFilename(caseName) => caseName :: accum
            case _ => accum
          }
        }.reverse
      case 1 =>
        List(args(0))
      case _ =>
        throw new RuntimeException("1st arg: name from input/*.json")
    }
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      val caseNames = oneCaseNameOrAllIfEmpty(args)
      doTemplateMatching(caseNames)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
