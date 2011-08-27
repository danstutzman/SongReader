import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D
import com.twitter.json.Json
import scala.io.Source
import scala.util.Random
import scala.util.Sorting

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
}

case class Segment (
  val y:Int,
  val x0:Int,
  val x1:Int
) {}

case class BoundingBox (
  val minX:Int,
  val maxX:Int,
  val minY:Int,
  val maxY:Int
) {}

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
  def separateNotes(input:GrayImage, caseName:String) = {
    val ceilings = new Array[Int](input.w)
    (0 until input.w).foreach { x =>
      val x0 = (x - 50) max 0
      val x1 = (x + 50) min (input.w - 1)
      val values = new Array[Int]((x1 - x0 + 1) * input.h)
      var i = 0
      (x0 to x1).foreach { xNeighbor =>
        (0 until input.h).foreach { y =>
          values(i) = input(xNeighbor, y)
          i += 1
        }
      }
      Sorting.quickSort(values)
      ceilings(x) = values(values.length * 1/4)
    }

    val floors = new Array[Int](input.w)
    (0 until input.w).foreach { x =>
      floors(x) = ceilings(x) * 1/2
    }

    val adjusted = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = input(x, y)
        val newV = (v - floors(x)) * 255 / (ceilings(x) - floors(x) + 1)
        adjusted(x, y) = if (newV < 0) 0 else if (newV > 255) 255 else newV
      }
    }
    adjusted.saveTo(new File("demos/adjusted.%s.png".format(caseName)))

    val augmentedBinaryNonStaff = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = input(x, y)
        val newV = (v - floors(x)) * 255 / (ceilings(x) - floors(x) + 1)
        val x0 = (x - 1) max 0
        val x1 = (x + 1) min (input.w - 1)
        val y0 = (y - 1) max 0
        val y1 = (y + 1) min (input.h - 1)
 
        val isCloseToFloor = (x0 to x1).exists { xNeighbor =>
          (y0 to y1).exists { yNeighbor =>
            input(xNeighbor, yNeighbor) <= floors(x)
          }
        }

        augmentedBinaryNonStaff(x, y) = if (isCloseToFloor) 0 else 255
      }
    }
    
    val partiallyErased = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        partiallyErased(x, y) =
          if (augmentedBinaryNonStaff(x, y) == 0) 255
          else adjusted(x, y)
      }
    }
    partiallyErased.saveTo(new File(
      "demos/partially_erased.%s.png".format(caseName)))

    (adjusted, partiallyErased, augmentedBinaryNonStaff)
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
    val demo = excerpt.resize(excerpt.w * 2, excerpt.h * 2, 0)
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
        if (y >= 0 && Math.round(y * 2).intValue < demo.h)
          demo(xUncentered * 2, Math.round(y * 2).intValue) = color

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

  def scaleTemplateColor(template:ColorImage, templateW:Int, templateH:Int) = {
    val templateScaled = new ColorImage(templateW, templateH)
    (0 until templateH).foreach { templateScaledY =>
      (0 until templateW).foreach { templateScaledX =>
        val y0 = templateScaledY * template.h / templateH
        val y1 =
          ((templateScaledY + 1) * template.h / templateH) min template.h
        val x0 = templateScaledX * template.w / templateW
        val x1 =
          ((templateScaledX + 1) * template.w / templateW) min template.w
        if (y1 > y0 && x1 > x0) {
          var (rSum, gSum, bSum) = (0, 0, 0)
          (y0 until y1).foreach { templateY =>
            (x0 until x1).foreach { templateX =>
              val (r, g, b) = template(templateX, templateY)
              rSum += r
              gSum += g
              bSum += b
            }
          }
          val rMean = rSum / (y1 - y0) / (x1 - x0)
          val gMean = gSum / (y1 - y0) / (x1 - x0)
          val bMean = bSum / (y1 - y0) / (x1 - x0)
          templateScaled(templateScaledX, templateScaledY) =
            (rMean, gMean, bMean)
        }
        else
          templateScaled(templateScaledX, templateScaledY) = (0, 0, 0)
      }
    }
    templateScaled
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
      case "TC" =>
        (staffSeparation * 3, staffSeparation * 3)
      case "44" =>
        (staffSeparation, staffSeparation * 2)
    }
    val (minH, maxH) = label match {
      case "L" | "S" | "Sa" | "Sb" | "2" =>
        (staffSeparation, staffSeparation * 3/2)
      case "#" =>
        (staffSeparation * 5/2, staffSeparation * 7/2)
      case "b" | "N" =>
        (staffSeparation * 2, staffSeparation * 4)
      case "TC" =>
        (staffSeparation * 8, staffSeparation * 9)
      case "44" =>
        (staffSeparation * 4, staffSeparation * 5)
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
    ((minW max 1) to maxW).foreach { templateW =>
    ((minH max 1) to maxH).foreach { templateH =>
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
                ((templateScaledX + 1) * templateSum.w / templateW - 1) max
                (templateFullX0 + 1)
              val templateFullY0 =
                templateScaledY * templateSum.h / templateH - 1
              val templateFullY1 =
                ((templateScaledY + 1) * templateSum.h / templateH - 1) max
                (templateFullY0 + 1)
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

/*
  def verticalHough(input:GrayImage, caseName:String) {
    val white = new GrayImage(input.h, input.w)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        val v = 255 - input(x, y)
        val closeToDark = (-3 to 3).exists { yNeighbor =>
          (-3 to 3).exists { xNeighbor =>
            (255 - input(x + xNeighbor, y + yNeighbor)) > 200
          }
        }
        white(y, x) = if (closeToDark) 0 else if (v < 100) 0 else (v - 100)
      }
    }
    white.saveTo(new File("demos/vertical.%s.png".format(caseName)))
    
    val hough = new GrayImage(40, input.w)
    (0 until white.h).foreach { y =>
      (0 until white.w).foreach { x =>
        val v = white(x, y)// - 150
        (-20 until 20).foreach { mCents =>
          val intercept = Math.round(y - (mCents / 30.0f * x)).intValue
          if (intercept >= 0 && intercept < hough.h)
            hough(mCents + 20, intercept) = hough(mCents + 20, intercept) + v
        }
      }
    }

    val max = hough.data.max
    (0 until hough.h).foreach { y =>
      (0 until hough.w).foreach { x =>
        hough(x, y) = hough(x, y) * 255 / max
      }
    }
    hough.saveTo(new File("demos/vhough.%s.png".format(caseName)))
  }
*/
  def eraseStaffLines(input:GrayImage, whereNotesAre:GrayImage,
      metrics:Metrics, yCorrection:Array[Float], caseName:String) = {
    val maxStaffSpacing =
        Math.abs(metrics.bSpacing) * (input.w / 2) + metrics.cSpacing
    val halfStaffSpacing = Math.ceil(maxStaffSpacing / 2).intValue

    val yNeighborToMedians = new Array[Array[Int]](halfStaffSpacing)
    (0 until halfStaffSpacing).foreach { i =>
      yNeighborToMedians(i) = new Array[Int](input.w)
    }

    (0 until input.w).foreach { xOuter =>
      val x0 = (xOuter - 50) max 0
      val x1 = (xOuter + 50) min (input.w - 1)
      val yNeighborToValues = new Array[List[Int]](halfStaffSpacing)
      (0 until halfStaffSpacing).foreach { i =>
        yNeighborToValues(i) = Nil
      }

      (-4 to 4 by 2).foreach { staffY =>
        (x0 to x1).foreach { x =>
          val xCentered = x - (input.w / 2)
          val a = metrics.a
          val b = metrics.b + (staffY / 2.0f * metrics.bSpacing)
          val c = metrics.c + (staffY / 2.0f * metrics.cSpacing)
          val y = Math.round((a * xCentered * xCentered + b * xCentered + c) +
            yCorrection(x) + (input.h / 2)).intValue
          (0 until halfStaffSpacing).foreach { yNeighbor =>
            val v = input(x, y + yNeighbor)
            if (whereNotesAre(x, y + yNeighbor) != 0)
              yNeighborToValues(yNeighbor) = v :: yNeighborToValues(yNeighbor)
          }
        }
      }
      
      (0 until halfStaffSpacing).foreach { yNeighbor =>
        val values = yNeighborToValues(yNeighbor).toArray
        Sorting.quickSort(values)
        val median = values(values.length / 2)
        yNeighborToMedians(yNeighbor)(xOuter) = median
      }
    }

    val demo = new ColorImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        // black means the darkest/middle color in the staff lines
        val black = yNeighborToMedians(0)(x)
        // white means the lightest color; the color outside the staff lines
        val white = yNeighborToMedians(halfStaffSpacing - 1)(x)

        // find out the distance to the closest staff line
        var minDistance = 9999
        (-4 to 4 by 2).foreach { staffY =>
          val xCentered = x - (input.w / 2)
          val a = metrics.a
          val b = metrics.b + (staffY / 2.0f * metrics.bSpacing)
          val c = metrics.c + (staffY / 2.0f * metrics.cSpacing)
          val yOfStaff =
            Math.round((a * xCentered * xCentered + b * xCentered + c) +
            yCorrection(x) + (input.h / 2)).intValue
          minDistance = minDistance min Math.abs(y - yOfStaff)
        }
        // if it's far from the staff (in ledger-line land), just use
        // the distance corresponding to half way between staff lines
        val distanceFromStaff = minDistance min (halfStaffSpacing - 1)

        val v = input(x, y)

        // predict the color at this pixel given its location relative
        // to the staff lines
        val expectedV = yNeighborToMedians(distanceFromStaff)(x)

        // was pixel darker than the staff line?  Positive diff means darker.
        val diff = expectedV - v
        val normalizedDiff = diff * 80 / ((white - black) max 10)
        val positiveDiff = if (diff > 0) normalizedDiff else 0

        // reverse and scale the pixel's color,
        // so its darkest black is full white (255)
        // and its lightest white is full black (0)
        val otherBlack = white / 2
        // the ink at this spot is a little darker than it would be if it
        // weren't overlapping a staff line, so subtract out the expected
        // darkness contribution of the staff line ink
        val v2 = v - (expectedV - white)
        val normalizedV1 =
          255 - ((v2 - otherBlack) * 255 / ((white - otherBlack) max 10))
        val normalizedV2 =
          if (normalizedV1 > 255) 255
          else if (normalizedV1 < 0) 0
          else normalizedV1

        demo(x, y) = (positiveDiff, 0, normalizedV2)
      }
    }
    demo.saveTo(new File("demos/erase.%s.png".format(caseName)))

    val demo2 = new GrayImage(input.w, input.h)
    val dx = 2
    val dy = 3
    val threshold = 25 // unit-less value since the differences were normalized
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        var sumRedness = 0
        val y0 = (y - dy) max 0
        val y1 = (y + dy) min (input.h - 1)
        val x0 = (x - dx) max 0
        val x1 = (x + dx) min (input.w - 1)
        (y0 to y1).foreach { yNeighbor =>
          (x0 to x1).foreach { xNeighbor =>
            sumRedness += demo(xNeighbor, yNeighbor)._1
          }
        }
        if (sumRedness / ((x1 - x0 + 1) * (y1 - y0 + 1)) > threshold)
          demo2(x, y) = 255 - demo(x, y)._3
        else
          demo2(x, y) = 255
      }
    }
    demo2.saveTo(new File("demos/erase2.%s.png".format(caseName)))

    // Draw staff lines on image to see if they look right
    val demo3 = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        demo3(x, y) = input(x, y)
      }
    }
    (0 until input.w).foreach { x =>
      val staffY = 0.0f
      val xCentered = x - (input.w / 2)
      val a = metrics.a
      val b = metrics.b + (staffY / 2.0f * metrics.bSpacing)
      val c = metrics.c + (staffY / 2.0f * metrics.cSpacing)
      val y = Math.round((a * xCentered * xCentered + b * xCentered + c) +
        yCorrection(x) + (input.h / 2)).intValue

      (0 until halfStaffSpacing).foreach { yInner =>
        demo3(x, y - yInner) = yNeighborToMedians(yInner)(x)
        demo3(x, y + yInner) = yNeighborToMedians(yInner)(x)
      }
    }
    demo3.saveTo(new File("demos/erase3.%s.png".format(caseName)))

    demo2
  }

  def scanSegments(input:GrayImage) : List[Segment] = {
    var segments:List[Segment] = Nil
    val background = 255
    (0 until input.h).foreach { y =>
      var previousPixel = background
      var startOfSegment = 0
      (0 until input.w).foreach { x =>
        val currentPixel = input(x, y)
        if (currentPixel != background) { // if new pixel isn't background,
          if (previousPixel == background) // ... but previous was ...
            startOfSegment = x // then start a new start
        }
        else { // if new pixel is the background,
          if (previousPixel != background) // ... and if the previous wasn't...
            segments = Segment(y, startOfSegment, x) :: segments // end segment
        }
        previousPixel = currentPixel
      }
    }
    segments
  }

  def groupTouchingSegments(segments:List[Segment]) : List[List[Segment]] = {
    case class SegmentGroup (
      var earlierLayers:List[Segment],
      var previousLayer:List[Segment],
      var currentLayer:List[Segment]
    ) {}

    var maxY = segments.foldLeft(0) { (maxY, segment) =>
      if (segment.y > maxY) segment.y else maxY
    }
    var yToSegments:Array[List[Segment]] = new Array[List[Segment]](maxY + 1)
    (0 to maxY).foreach { y => yToSegments(y) = Nil }
    segments.foreach { segment =>
      yToSegments(segment.y) = segment :: yToSegments(segment.y)
    }

    var activeGroups:List[SegmentGroup] = Nil
    var inactiveGroups:List[SegmentGroup] = Nil
    (0 to maxY).foreach { y =>
      yToSegments(y).foreach { segment =>
        val touchingGroups = activeGroups.filter { group =>
          group.previousLayer.find { previousSegment =>
            segment.x0 < previousSegment.x1 && segment.x1 > previousSegment.x0
          }.isDefined
        }
        val assignedGroup = touchingGroups match {
          case Nil =>
            val newGroup = SegmentGroup(Nil, Nil, Nil)
            activeGroups = newGroup :: activeGroups
            newGroup
          case matchedGroup :: otherGroups =>
            activeGroups = activeGroups -- (matchedGroup :: otherGroups)
            val mergedGroup =
                otherGroups.foldLeft(matchedGroup) { (accum, toAdd) =>
              SegmentGroup(
                accum.earlierLayers ++ toAdd.earlierLayers,
                accum.previousLayer ++ toAdd.previousLayer,
                accum.currentLayer ++ toAdd.currentLayer)
            }
            activeGroups = mergedGroup :: activeGroups
            mergedGroup
        }
        assignedGroup.currentLayer = segment :: assignedGroup.currentLayer
      }

      val (newActiveGroups, newInactiveGroups) =
        activeGroups.partition { group => group.currentLayer.size > 0 }
      inactiveGroups = newInactiveGroups ::: inactiveGroups
      activeGroups = newActiveGroups.map { group =>
        SegmentGroup(
          group.previousLayer ::: group.earlierLayers,
          group.currentLayer,
          Nil)
      }
    }

    (inactiveGroups ::: activeGroups).map { group =>
      group.earlierLayers ::: group.previousLayer ::: group.currentLayer
    }
  }

  def demoSegmentGroups(segmentGroups:List[List[Segment]], 
      justNotes:GrayImage, caseName:String) {
    val demo = new ColorImage(justNotes.w, justNotes.h)
    val random = new Random(0)
    segmentGroups.foreach { segmentGroup =>
      val color =
        (random.nextInt(256), random.nextInt(256), random.nextInt(256))
      segmentGroup.foreach { segment =>
        (segment.x0 until segment.x1).foreach { x =>
          demo(x, segment.y) = color
        }
      }
    }
    demo.saveTo(new File("demos/segment_groups.%s.png".format(caseName)))
  }

/*
  def fitOvalsInSegmentGroups(segmentGroups:List[List[Segment]], 
      justNotes:GrayImage, caseName:String) {
    val w = 11
    val h = 11
    val yToOvalWidth = (0 until h).map { y =>
      (Math.sqrt(1.0 - Math.pow(((y + 0.5) / h) * 2 - 1, 2)) * w).intValue
    }
    val demo = justNotes.toColorImage
    segmentGroups.foreach { segmentGroup =>
      val minX = segmentGroup.foldLeft(segmentGroup(0).x0) { _ min _.x0 }
      val maxX = segmentGroup.foldLeft(segmentGroup(0).x1) { _ max _.x1 }
      val minY = segmentGroup.foldLeft(segmentGroup(0).y) { _ min _.y }
      val maxY = segmentGroup.foldLeft(segmentGroup(0).y) { _ max _.y }
      val yToSegments = (minY to maxY).map { y =>
        segmentGroup.filter { _.y == y }
      }

      var centerXSegments:List[Segment] = Nil
      (minY to maxY - h).foreach { topY =>
        var possibleCenterXs = (minX to maxX).toList
        (0 until h).foreach { ovalY =>
          val segmentsOnThisLine = yToSegments(topY + ovalY - minY)
          val ovalWidth = yToOvalWidth(ovalY)
          possibleCenterXs = possibleCenterXs.filter { centerX =>
            segmentsOnThisLine.find { segment =>
              val x0 = segment.x0 + ovalWidth/2
              val x1 = segment.x1 - ovalWidth/2
              centerX >= x0 && centerX <= x1
            }.isDefined
          }
        }

        possibleCenterXs.foreach { centerX =>
          demo(centerX, topY + h/2) = (0, 0, 255)
        }

        var previousX = -1
        var startOfSegment = possibleCenterXs.min
        var newCenterXSegments:List[Segment] = Nil
        val centerY = topY + h/2
        possibleCenterXs.toList.sort.foreach { x =>
          if (x > previousX + 1) {
            newCenterXSegments = Segment(centerY, startOfSegment, previousX) ::
              newCenterXSegments
          }
        }
      }
    }
    demo.saveTo(new File("demos/center_xs.%s.png".format(caseName)))
  }
*/

  def boundSegmentGroups(segmentGroups:List[List[Segment]]) = {
    segmentGroups.map { segments =>
      val minX = segments.foldLeft(segments(0).x0) { (minX, segment) =>
        if (segment.x0 < minX) segment.x0 else minX
      }
      val maxX = segments.foldLeft(segments(0).x1) { (maxX, segment) =>
        if (segment.x1 > maxX) segment.x1 else maxX
      }
      val minY = segments.foldLeft(segments(0).y) { (minY, segment) =>
        if (segment.y < minY) segment.y else minY
      }
      val maxY = segments.foldLeft(segments(0).y) { (maxY, segment) =>
        if (segment.y > maxY) segment.y else maxY
      }
      BoundingBox(minX, maxX, minY, maxY)
    }
  }

  def demoBounds(baseImage:ColorImage, bounds:List[BoundingBox],
      caseName:String) {
    val demo = baseImage.copy
    bounds.foreach { bound =>
      val color = (255, 0, 0)
      (bound.minX to bound.maxX).foreach { x =>
        demo(x, bound.minY) = color
        demo(x, bound.maxY) = color
      }
      (bound.minY to bound.maxY).foreach { y =>
        demo(bound.minX, y) = color
        demo(bound.maxX, y) = color
      }
    }
    demo.saveTo(new File("demos/bounds.%s.png".format(caseName)))
  }

  case class Transform (
    val targetW:Int,
    val targetH:Int,
    val targetMidX:Int,
    val targetMidY:Int,
    val blurTenths:Int,
    val brightenTenths:Int
  ) {}

  def shrinkHalfSize(input:GrayImage) = {
    // add one before halving because we want to round up not down
    // when you access a pixel that's out of bounds, it returns 0;
    // otherwise this technique wouldn't work
    val output = new GrayImage((input.w + 1) / 2, (input.h + 1) / 2)
    (0 until output.h).foreach { y =>
      (0 until output.w).foreach { x =>
        val v = (input(x*2, y*2    ) + input(x*2 + 1, y*2) +
                 input(x*2, y*2 + 1) + input(x*2 + 1, y*2 + 1)) / 4
        output(x, y) = v
      }
    }
    output
  }

  def matchTrebleTemplate(justNotes:GrayImage, metrics:Metrics,
      caseName:String) {
    val template = ColorImage.readFromFile(new File(
      "templates/treble_clef.png")).toGrayImage
    val templateSum = sumTemplate(template.inverse)

    val halfSize = shrinkHalfSize(justNotes.inverse)
    //halfSize.saveTo(new File("demos/half_size.png"))
    val quarterSize = shrinkHalfSize(halfSize)
    //quarterSize.saveTo(new File("demos/quarter_size.png"))
    val eighthSize = shrinkHalfSize(quarterSize)
    //eighthSize.saveTo(new File("demos/eighth_size.png"))

    case class TM ( // TemplateMatch
      val w:Int,
      val h:Int,
      val x0:Int,
      val y0:Int,
      val brightenTenths:Int,
      val meanDiff:Int
    ) {}

    def shrinkTemplate(templateW:Int, templateH:Int) = {
      val smallTemplate = new GrayImage(templateW, templateH)
      (0 until templateH).foreach { y =>
        (0 until templateW).foreach { x =>
          val templateX0 = x * template.w / templateW - 1
          val templateX1 = (x + 1) * template.w / templateW - 1
          val templateY0 = y * template.h / templateH - 1
          val templateY1 = (y + 1) * template.h / templateH - 1
          val templateV = (
            templateSum(templateX1, templateY1) -
            templateSum(templateX1, templateY0) -
            templateSum(templateX0, templateY1) +
            templateSum(templateX0, templateY0)) /
            ((templateX1 - templateX0) * (templateY1 - templateY0))
          smallTemplate(x, y) = templateV
        }
      }
      smallTemplate
    }

          //(0 until currentSize.h - templateH).foreach { inputY0 =>
           // (0 until currentSize.w - templateW).foreach { inputX0 =>
    def makeMatches(input:GrayImage, wRange:(Int,Int), hRange:(Int,Int),
        x0Range:(Int,Int), y0Range:(Int,Int)) = {
      var tms:List[TM] = Nil
      (hRange._1 to hRange._2).foreach { templateH =>
        (wRange._1 to wRange._2).foreach { templateW =>
          val smallTemplate = shrinkTemplate(templateW, templateH)
          val newX0Range =
            (x0Range._1 max 0, x0Range._2 min (input.w - templateW))
          val newY0Range =
            (y0Range._1 max 0, y0Range._2 min (input.h - templateH))
          (newY0Range._1 to newY0Range._2).foreach { inputY0 =>
            (newX0Range._1 to newX0Range._2).foreach { inputX0 =>
              var minMeanDiff = 999999
              var bestBrightenTenths = 0
              (6 to 20 by 1).foreach { brightenTenths =>
                var sumDiff = 0
                (0 until templateH).foreach { y =>
                  (0 until templateH).foreach { x =>
                    val inputV = input(inputX0 + x - y/5, inputY0 + y)
                    val templateV =
                      (smallTemplate(x, y) * brightenTenths / 10) min 255
                    //sumDiff += Math.abs(inputV - templateV)
                    sumDiff += (inputV - templateV) * (inputV - templateV)
                  }
                }
                val meanDiff = sumDiff / (templateW * templateH)
                if (meanDiff < minMeanDiff) {
                  minMeanDiff = meanDiff
                  bestBrightenTenths = brightenTenths
                }
              }
              val newTM = TM(templateW, templateH, inputX0, inputY0,
                bestBrightenTenths, minMeanDiff)
              tms = newTM :: tms
            }
          }
        }
      }
      tms.toArray
    }

    /*val eighthTms = makeMatches(
      //eighthSize, (4,4), (6,10), (0,eighthSize.w-1), (0,eighthSize.h-1))
      eighthSize, (3,5), (6,10), (0,5), (0,5))
    val sorted = eighthTms.sortBy { _.meanDiff }
    //println(sorted(0))
    val filteredEighthTms = eighthTms.filter { tm => tm.meanDiff < 40 }
    //filteredEighthTms.foreach { tm => println(tm) }*/


    def drawMatch(output:ColorImage, _match:TM) {
      val TM(bestW, bestH, bestX0, bestY0, bestBrightenTenths, bestMeanDiff) =
        _match
      val smallTemplate = shrinkTemplate(bestW, bestH)
      (0 until smallTemplate.h).foreach { y =>
        (0 until smallTemplate.w).foreach { x =>
          val (r, g, b) = output(bestX0 + x - y/5, bestY0 + y)
          val templateV =
            (smallTemplate(x, y) * bestBrightenTenths / 10) min 255
          val diff =
            if (templateV > r)
              (templateV - r, 0, 0)
            else
              (0, 0, r - templateV)
          output(bestX0 + x - y/5, bestY0 + y) = diff //(templateV, templateV, templateV)//g, b)
        }
      }
    }
    //filteredEighthTms.foreach { match_ =>
//    currentSize.saveTo(new File("demos/treble.%s.png".format(caseName)))

    val minHeight = (metrics.cSpacing / 4 * 6).intValue
    val maxHeight = (metrics.cSpacing / 4 * 9).intValue
    println(("minHeight", minHeight, "maxHeight", maxHeight))
    val quarterTms = makeMatches(
      //eighthSize, (4,4), (6,10), (0,eighthSize.w-1), (0,eighthSize.h-1))
      //quarterSize, (4,6), (11,13), (5,15), (2,8))
      //quarterSize, (minHeight / 2,maxHeight / 2), (minHeight,maxHeight),
      quarterSize, (9, 10), (20, 22),
      //(0,quarterSize.w), (0,quarterSize.h))
      (12,14), (3,5))
    val sorted = quarterTms.sortBy { _.meanDiff }
    println(sorted(0))
    //val filteredQuarterTms = quarterTms.filter { tm => tm.meanDiff < 17100 }
    val filteredQuarterTms = List(sorted(0))
    filteredQuarterTms.foreach { tm => println(tm) }

    def doubleMatch(_match:TM) = {
      val TM(w, h, x0, y0, brightenTenths, meanDiff) = _match
      TM(w * 2, h * 2, x0 * 2, y0 * 2, brightenTenths, meanDiff)
    }

    val demo = quarterSize.toColorImage
    filteredQuarterTms.foreach { quarterMatch =>
      drawMatch(demo, quarterMatch)
    }
    demo.saveTo(new File("demos/treble.%s.png".format(caseName)))

/*
    val quarterMatches = filteredEighthTms.map { eighthMatch =>
      val TM(w, h, x0, y0, brightenTenths, meanDiff) = doubleMatch(eighthMatch)
      val allMatches = makeMatches(quarterSize,
        (w-2,w+2), (h-2,h+2), (x0-2,x0+2), (y0-2,y0+2))

      var bestMatch = TM(0,0,0,0,0,999999)
      allMatches.foreach { match_ =>
        if (match_.meanDiff < bestMatch.meanDiff)
          bestMatch = match_
      }
      bestMatch
    }
    val filteredQuarterMatches = quarterMatches.filter { _.meanDiff < 99999 }
*/

    /*val demo = quarterSize.toColorImage
    filteredQuarterMatches.foreach { match_ =>
      drawMatch(demo, match_)
    }
    demo.saveTo(new File("demos/treble.%s.png".format(caseName)))*/

/*
    val halfMatches = filteredQuarterMatches.map { quarterMatch =>
      val TM(w, h, x0, y0, brightenTenths, meanDiff) = doubleMatch(quarterMatch)
      val allMatches = makeMatches(halfSize,
        (w-2,w+2), (h-2,h+2), (x0-2,x0+2), (y0-2,y0+2))

      var bestMatch = TM(0,0,0,0,0,999999)
      allMatches.foreach { match_ =>
        if (match_.meanDiff < bestMatch.meanDiff)
          bestMatch = match_
      }
      bestMatch
    }
    val filteredHalfMatches = halfMatches.filter { _.meanDiff < 999999 }
*/

    /*val demo = halfSize.toColorImage
    filteredHalfMatches.foreach { match_ =>
      drawMatch(demo, match_)
    }
    demo.saveTo(new File("demos/treble.%s.png".format(caseName)))*/

/*
    val fullMatches = filteredHalfMatches.map { halfMatch =>
      val TM(w, h, x0, y0, brightenTenths, meanDiff) = doubleMatch(halfMatch)
      val allMatches = makeMatches(justNotes.inverse,
        (w-2,w+2), (h-2,h+2), (x0-2,x0+2), (y0-2,y0+2))

      var bestMatch = TM(0,0,0,0,0,999999)
      allMatches.foreach { match_ =>
        if (match_.meanDiff < bestMatch.meanDiff)
          bestMatch = match_
      }
      bestMatch
    }
    val filteredFullMatches = fullMatches.filter { _.meanDiff < 999999 }
*/

    /*val demo = justNotes.inverse.toColorImage
    filteredFullMatches.foreach { match_ =>
      drawMatch(demo, match_)
    }
    demo.saveTo(new File("demos/treble.%s.png".format(caseName)))*/

/*
    filteredEighthTms.foreach { eighthMatch =>
      val TM(bestW, bestH, bestX0, bestY0, bestMeanDiff) = match_
      val smallTemplate = shrinkTemplate(bestW * 2, bestH * 2).inverse
      (0 until smallTemplate.h).foreach { y =>
        (0 until smallTemplate.w).foreach { x =>
          quarterSize(bestX0 * 2 + x, bestY0 * 2 + y) = smallTemplate(x, y)
        }
      }
    }*/

/*
    //val (x0, y0, x1, y1) = (20, 24, 50, 88)
    //val (x0, y0, x1, y1) = (20 / 2, 24 / 2, 50 / 2, 88 / 2)
    //val (x0, y0, x1, y1) = (20 / 4, 24 / 4, 50 / 4, 88 / 4)
    val (x0, y0, x1, y1) = (20 / 8, 24 / 8, 50 / 8, 88 / 8)
    //val demo = justNotes.toColorImage
    //val demo = shrinkHalfSize(justNotes).toColorImage
    //val demo = shrinkHalfSize(shrinkHalfSize(justNotes)).toColorImage
    val demo =
      shrinkHalfSize(shrinkHalfSize(shrinkHalfSize(justNotes))).toColorImage

    def applyTransform(t:Transform)(block:((Int,Int,Int) => Unit)) = {
      (y0 to y1).foreach { sourceY =>
        (x0 to x1).foreach { sourceX =>
          val Transform(targetW, targetH, targetMidX, targetMidY,
               blurTenths_, brightenTenths_) = t
          val blurTenths = 0
          val brightenTenths = 10

          val targetX0 = targetMidX - targetW / 2
          val targetY0 = targetMidY - targetH / 2
          val templateX0 =
            Math.round((sourceX - x0 - blurTenths/10.0) *
            template.w / targetW - targetX0).intValue
          val templateX1 =
            Math.round((sourceX - x0 + 1 + blurTenths/10.0) *
            template.w / targetW - targetX0).intValue
          val templateY0 =
            Math.round((sourceY - y0 - blurTenths/10.0) *
            template.h / targetH - targetY0).intValue
          val templateY1 =
            Math.round((sourceY - y0 + 1 + blurTenths/10.0) *
            template.h / targetH - targetY0).intValue
          val templateX0New = (templateX0 max 0) min (template.w - 1)
          val templateX1New = (templateX1 max 0) min (template.w - 1)
          val templateY0New = (templateY0 max 0) min (template.h - 1)
          val templateY1New = (templateY1 max 0) min (template.h - 1)
          val templateVSum =
            templateSum(templateX1New, templateY1New) -
            templateSum(templateX0New, templateY1New) -
            templateSum(templateX1New, templateY0New) +
            templateSum(templateX0New, templateY0New)
          val denom = (templateX1 - templateX0) * (templateY1 - templateY0)
          val templateV = templateVSum / (denom max 1)
          val templateVNew =
            (templateV * brightenTenths / 10) min 255

          block(sourceX, sourceY, templateVNew)
        }
      }
    }

    def setTargetW(t0:Transform, x:Int) = {
      Transform(t0.targetW + x, t0.targetH, t0.targetMidX, t0.targetMidY,
        t0.blurTenths, t0.brightenTenths)
    }
    def setTargetH(t0:Transform, x:Int) = {
      Transform(t0.targetW, t0.targetH + x, t0.targetMidX, t0.targetMidY,
        t0.blurTenths, t0.brightenTenths)
    }
    def setTargetX(t0:Transform, x:Int) = {
      Transform(t0.targetW, t0.targetH, t0.targetMidX + x, t0.targetMidY,
        t0.blurTenths, t0.brightenTenths)
    }
    def setTargetY(t0:Transform, x:Int) = {
      Transform(t0.targetW, t0.targetH, t0.targetMidX, t0.targetMidY + x,
        t0.blurTenths, t0.brightenTenths)
    }
    def setBlurTenths(t0:Transform, x:Int) = {
      Transform(t0.targetW, t0.targetH, t0.targetMidX, t0.targetMidY,
        t0.blurTenths + x, t0.brightenTenths)
    }
    def setBrightenTenths(t0:Transform, x:Int) = {
      Transform(t0.targetW, t0.targetH, t0.targetMidX, t0.targetMidY,
        t0.blurTenths, t0.brightenTenths + x)
    }

    case class TRange (
      val setter:((Transform,Int)=>Transform),
      val min:Int,
      val max:Int
    ) {}

    case class SetterPair (
      val first:(Transform,Int)=>Transform,
      val second:(Transform,Int)=>Transform
    )

    def findBestTransformFor(initialTransform:Transform,
        setterPairs:List[SetterPair])(transformScorer:(Transform=>Int)) = {
      var minDiff = 999999
      var bestTransform = initialTransform
      var numIterations = 0
      (1 to 6).foreach { i =>
        setterPairs.foreach { setterPair =>
          val SetterPair(setter1, setter2) = setterPair
          List(5, 2, 1).foreach { changeAmount =>
            var newBestTransform = bestTransform
            List(-changeAmount, 0, changeAmount).foreach { change1 =>
              List(-changeAmount, 0, changeAmount).foreach { change2 =>
                val newTransform1 = setter1(bestTransform, change1)
                val newTransform2 = setter2(newTransform1, change2)
                val diff = transformScorer(newTransform2)
                numIterations += 1
                if (diff < minDiff) {
                  minDiff = diff
                  newBestTransform = newTransform2
                }
              }
            }
            bestTransform = newBestTransform
          }
        }
      }
      (bestTransform, minDiff, numIterations)
    }

    val setters:List[SetterPair] = List(
      SetterPair(setTargetW, setTargetH),
      SetterPair(setTargetX, setTargetY),
      SetterPair(setBlurTenths, setBrightenTenths)
    )

//(bestTransform,Transform(25,62,17,11,19,12))
//(minDiff,52383)

    val t0 = Transform(x1 - x0, y1 - y0, (x0 + x1) / 2, (y0 + y1) / 2, 10, 10)
    val (bestTransform, minDiff, numIterations) =
        findBestTransformFor(t0, setters) { transform =>
      var diff = 0
      applyTransform(transform) { (sourceX, sourceY, templateV) =>
        val (r, g, b) = demo(sourceX, sourceY)
        diff += Math.abs((255 - r) - templateV)
      }
      diff
    }

*/
/*
    var minDiff = 999999
    var bestTransform = Transform(0, 0, 0, 0, 0, 0)
    var numTests = 0
    (25 to 25).foreach { targetW =>
    (62 to 62).foreach { targetH =>
    (10 to 20).foreach { targetX =>
    (11 to 11).foreach { targetY =>
    (19 to 19).foreach { blurTenths =>
    (12 to 12).foreach { brightenTenths =>
      var transform = Transform(
        targetW, targetH, targetX, targetY, blurTenths, brightenTenths)
      var diff = 0
      applyTransform(transform) { (sourceX, sourceY, templateV) =>
        val (r, g, b) = demo(sourceX - x0 + (x1 - x0), sourceY - y0)
        diff += Math.abs(r - templateV)
      }
      numTests += 1
      if (diff < minDiff) {
        minDiff = diff
        bestTransform = transform
      }
    }}}}}}
*/
/*

    println(("bestTransform", bestTransform))
    println(("minDiff by pixel",
      minDiff / bestTransform.targetW / bestTransform.targetH))
    println(("numIterations", numIterations))

    applyTransform(bestTransform) { (sourceX, sourceY, templateV) =>
      val (r, g, b) = demo(sourceX, sourceY)
      demo(sourceX, sourceY) = (templateV, 0, 0)
    }

    demo.saveTo(new File("demos/treble.%s.png".format(caseName)))
*/
  }

  def rainbow(input:GrayImage, range:Int) = {
    val output = new ColorImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        var sumX = 0
        var sumY = 0
        (-range to range).foreach { xNeighbor =>
          (-range to range).foreach { yNeighbor =>
            val v = input(x + xNeighbor, y + yNeighbor)
            val xSign =
              if (xNeighbor == 0) 0 else (xNeighbor / Math.abs(xNeighbor))
            val ySign =
              if (yNeighbor == 0) 0 else (yNeighbor / Math.abs(yNeighbor))
            sumX += v * xSign
            sumY += v * ySign
          }
        }
        var meanX = sumX / (range * 2) / (range * 2 + 1)
        var meanY = sumY / (range * 2) / (range * 2 + 1)

        val norm = Math.sqrt(meanX * meanX + meanY * meanY)
        output(x, y) =
          if (norm == 0)
            (0, 0, 0)
          else
            ((meanX * 100 / norm).intValue + 100,
             (meanY * 100 / norm).intValue + 100, norm.intValue)

        /*var angle255 =
          ((Math.atan2(meanY, meanX) + Math.PI) / Math.PI * 127).intValue
        output(x, y) = (angle255, if (norm > 0.0) 255 else 0, 0)*/

        /*if (meanX == 0 && meanY == 0) {
          if (input(x, y) > 128)
            output(x - x0, y - y0) = (255, 255, 255)
          else
            output(x - x0, y - y0) = (0, 0, 0)
        }
        else {
          var angle255 =
            ((Math.atan2(meanY, meanX) + Math.PI) / Math.PI * 127).intValue
          //var r = 128 - Math.abs(angle255 - 128)
          //var b = 255 - r
          var (r, g, b) =
            if (angle255 < 64)
              (255, angle255 * 4, 0)
            else if (angle255 < 128)
              ((127 - angle255) * 4, 255, 0)
            else if (angle255 < 192)
              (0, (191 - angle255) * 4, (angle255 - 128) * 4)
            else
              ((angle255 - 192) * 4, 0, (255 - angle255) * 4)
          output(x - x0, y - y0) = (r, g, b)
        }*/
      }
    }
    output
  }

  def colorTrebleTemplate(justNotes:GrayImage, caseName:String) {
    val input = ColorImage.readFromFile(
      new File("templates/treble_clef.png")).toGrayImage.inverse
    val range = 13
    val (x0, y0, x1, y1) =
      (0 - range, 0 - range, input.w + range, input.h + range)

    val demo = new ColorImage(x1 - x0 + 1, y1 - y0 + 1)
    demo.saveTo(new File("demos/color_treble.%s.png".format(caseName)))
  }

  def trebleHough(justNotes:GrayImage, staffSeparation:Int, caseName:String) {
    val (templateW, templateH) = (staffSeparation * 5, staffSeparation * 8)
    val bigTemplate = ColorImage.readFromFile(new File(
      "templates/treble_clef.png")).toGrayImage.inverse
    val template = scaleTemplate(bigTemplate, templateW, templateH)
    val hough = new GrayImage(justNotes.w, justNotes.h)
    val input = justNotes.inverse

    val bigTemplateRainbow = rainbow(bigTemplate, 13)
    val templateRainbow =
      scaleTemplateColor(bigTemplateRainbow, templateW, templateH)
    templateRainbow.saveTo(new File(
      "demos/template_rainbow.%s.png".format(caseName)))
    val inputRainbow = rainbow(input, 3)
    inputRainbow.saveTo(new File(
      "demos/input_rainbow.%s.png".format(caseName)))

    (0 until input.w).foreach { inputX =>
      (0 until input.h).foreach { inputY =>
        val (inputR, inputG, inputB) = inputRainbow(inputX, inputY)
        (0 until template.w).foreach { templateX =>
          (0 until template.h).foreach { templateY =>
            val (templateR, templateG, templateB) =
              templateRainbow(templateX, templateY)
            val scored = (templateR > 0 && templateG > 0 &&
              inputR > 0 && inputG > 0 &&
              Math.abs(templateR - inputR) < 2 &&
              Math.abs(templateG - inputG) < 2)
              //inputB > 40)

            val houghX = inputX - (templateX - template.w/2)
            val houghY = inputY - (templateY - template.h/2)
            if (scored &&
                houghX >= 0 && houghX < hough.w &&
                houghY >= 0 && houghY < hough.h) {
              hough(houghX, houghY) = hough(houghX, houghY) + 1
            }
          }
        }
      }
    }


    // Scale down the hough image so it's between 0-255
    val max = hough.data.max 
    if (max > 0) {
      (0 until hough.w).foreach { x =>
        (0 until hough.h).foreach { y =>
          hough(x, y) = hough(x, y) * 255 / max
        }
      }
    }
    println(("max of hough", max))
    println(("normalized max of hough", max / template.w / template.h))

    hough.saveTo(new File("demos/treble_hough.%s.png".format(caseName)))
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
    val templateTrebleClef =
      ColorImage.readFromFile(new File("templates/treble_clef.png")).toGrayImage
    val template44 =
      ColorImage.readFromFile(new File("templates/44.png")).toGrayImage

    var globalPerformance = Performance(List(), List(), List())
    caseNames.foreach { caseName =>
      val imageFilename = "input/%s.jpeg".format(caseName)
      val input =
        ColorImage.readFromFile(new File(imageFilename)).toGrayImage
      val annotation = loadAnnotationJson(caseName)

      val (inputAdjusted, partiallyErased, augmentedBinaryNonStaff) =
        separateNotes(input, caseName)
      val metrics = estimateMetrics(partiallyErased, caseName)
      val yCorrection = determineYCorrection(
        partiallyErased, augmentedBinaryNonStaff, metrics, caseName)
      val justNotes = eraseStaffLines(input, augmentedBinaryNonStaff,
        metrics, yCorrection, caseName)

      //matchTrebleTemplate(justNotes, metrics, caseName)
      //colorTrebleTemplate(justNotes, caseName)
      trebleHough(justNotes, metrics.cSpacing.intValue, caseName)
/*
      val segments = scanSegments(justNotes)
      val segmentGroups = groupTouchingSegments(segments)
      demoSegmentGroups(segmentGroups, justNotes, caseName)
      demoStaffLines(inputAdjusted, metrics, yCorrection, caseName)

      var points:List[TemplateMatch] = Nil
      List("L", "2", "#", "TC", "44").foreach { label =>
      //List("#").foreach { label =>
        println(label)
        val templateSum = label match {
          case "L" => sumTemplate(templateBlackHead)
          case "2" => sumTemplate(templateWhiteHead)
          case "#" => sumTemplate(templateSharp)
          case "TC" => sumTemplate(templateTrebleClef)
          case "44" => sumTemplate(template44)
        }

        (-8 to 8).foreach { staffY =>
          (0 until augmentedBinaryNonStaff.w).foreach { x =>
            val xCentered = x - (inputAdjusted.w / 2)
            val a = metrics.a
            val b = metrics.b + (staffY / 2.0f * metrics.bSpacing)
            val c = metrics.c + (staffY / 2.0f * metrics.cSpacing)
            val y = Math.round((a * xCentered * xCentered + b * xCentered + c) +
              yCorrection(x) + (inputAdjusted.h / 2)).intValue

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
          case "#" => point1.blackMatch > 50 && point1.whiteMatch > 100
          case "TC" => point1.blackMatch > 40 && point1.whiteMatch > 130
          case "44" => point1.blackMatch > 70 && point1.whiteMatch > 100
        }

        !hasBetterNeighbor && strongEnough
      }

      val overlappingPointGroups = groupOverlappingPoints(pointsFiltered)
      demoAlternatives(overlappingPointGroups, inputAdjusted, caseName)
      demoPointGroups(overlappingPointGroups, inputAdjusted, caseName)

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

      val demo = inputAdjusted.toColorImage
      filteredNotes.foreach { noteGroup =>
        noteGroup.foreach { point =>
          val template = point.label match {
            case "L" => templateBlackHead
            case "2" => templateWhiteHead
            case "#" => templateSharp
            case "TC" => templateTrebleClef
            case "44" => template44
          }
          drawTemplateMatch(point, demo, template)
        }
      }
      demo.saveTo(new File("demos/notes.%s.png".format(caseName)))

      val realNotes = filteredNotes.map { _.filter { note =>
        note.label == "L" || note.label == "2" } }
      demoNotes(realNotes, inputAdjusted.toColorImage, caseName)
      val performance = calcPerformance(realNotes, annotation.notes)
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
*/
    }
    println("Total:   precision: %.3f -- recall: %.3f".format(
      globalPerformance.precision, globalPerformance.recall))
  }

  def expandCaseNames(args:Array[String]) = {
    var caseNames:List[String] = Nil
    val filenames = new File("input").listFiles
    args.foreach { arg =>
      val GlobMatch = ("(" + arg.replaceAll("\\*", ".*") + ")\\.json").r
      filenames.foreach { filename =>
        filename.getName match {
          case GlobMatch(caseName) => caseNames = caseName :: caseNames
          case _ => ()
        }
      }
    }
    if (args.length == 0)
      throw new RuntimeException("1st arg: case name from input/*.json")
    caseNames.reverse
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      val caseNames = expandCaseNames(args)
      doTemplateMatching(caseNames)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
