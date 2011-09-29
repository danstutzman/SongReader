import com.twitter.json.Json
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.util.Random
import scala.util.Sorting

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
  val ansiEscapeNormal  = "\u001b" + "[0m"
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

case class Beam (
  val x0:Int,
  val x1:Int,
  val y0:Int,
  val y1:Int
) {}

case class LabeledPoint (
  val label:String,
  val x:Int,
  val y:Int,
  val staffY:Int
) {}

case class Annotation (
  val points:List[LabeledPoint],
  val notes:List[Set[Int]]
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
  val staffY:Int,
  val templateName:String
) {}

case class TemplateSpec (
  val name:String,
  val widthInStaffLines:Double,
  val heightInStaffLines:Double,
  val finder:(GrayImage,GrayImage,String)=>GrayImage,
  val threshold:Double
) {}

case class Orthonormal (
  val image:GrayImage,
  val yForStaffY:Map[Int,Int],
  val xForXIntercept:Array[Int],
  val cSpacing:Float, // equal to Metrics.cSpacing (units haven't changed)
  val transformXY:(Int,Int)=>(Int,Int)
) {}

case class VLine (
  val xIntercept:Int,
  val y0:Int,
  val y1:Int
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
      noteGroups:List[Set[TemplateMatch]], input:ColorImage, caseName:String) {
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
    var noteGroups:List[List[TemplateMatch]] = Nil
    var currentNoteGroup:List[TemplateMatch] = Nil
    var lastNoteX = -1
    val matchesSorted = matches.sortBy { _.x }
    matchesSorted.foreach { _match =>
      if (Math.abs(_match.x - lastNoteX) >= 20 && currentNoteGroup.size > 0) {
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

  def loadAnnotationJson(annotationString : String) : Annotation = {
    val annotationJson = Json.parse(annotationString)
    val points = annotationJson.asInstanceOf[List[Map[String,Any]]].map {
        pointJson => LabeledPoint(
          pointJson("type").asInstanceOf[String],
          pointJson("x").asInstanceOf[Int],
          pointJson("y").asInstanceOf[Int],
          pointJson("staffY").asInstanceOf[Int]
        )
      }

    var noteGroups:List[Set[Int]] = Nil
    var currentNoteGroup = Set[Int]()
    var lastNoteX = -999
    val realPoints = points.filter { point =>
      Set("8", "4", "2", "1").contains(point.label)
    }
    realPoints.sortBy { _.x }.foreach { point =>
      if (Math.abs(point.x - lastNoteX) >= 20 && currentNoteGroup.size > 0) {
        noteGroups = currentNoteGroup :: noteGroups
        currentNoteGroup = Set[Int]()
      }

      currentNoteGroup += point.staffY

      lastNoteX = point.x
    }
    if (currentNoteGroup.size > 0)
      noteGroups = currentNoteGroup :: noteGroups
    val notes = noteGroups.reverse

    Annotation(points, notes)
  }

  // Levenshtein minimum-edit-distance to determine best alignment
  // between detected note groups and ground truth note groups.
  // This ensures that a spurious or missed note only causes one
  // error instead of throwing off all the notes to the right.
  def calcPerformance(
      estimatedNotes:List[Set[TemplateMatch]], annotated:List[Set[Int]]) = {
    //printf("init estimated: %s\n", estimatedNotes.map { _.map { _.staffY } })
    //printf("init annotated: %s\n", annotated)

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
          val scoreIncrease = estimated(x - 1).size
          val score = matrix(y)(x - 1) + scoreIncrease
          possibilities = Possibility((-1, 0), score) :: possibilities
        }
        if (y > 0) {
          // If this noteGroup is totally missing from prediction
          val scoreIncrease = annotated(y - 1).size
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

    //matrix.foreach { row =>
    //  row.foreach { v =>
    //    printf("%3d", v)
    //  }
    //  printf("\n")
    //}

    case class PairedNoteGroup (
      val staffX:Int,
      val estimatedNotes:Set[TemplateMatch],
      val annotated:Set[Int]
    ) {}
    var pairedNoteGroups:List[PairedNoteGroup] = Nil
    var x = w
    var y = h
    while (x > 0 || y > 0) {
      backPointer(y)(x) match {
        case (-1, 0) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimatedNotes(x - 1), Set()) ::
            pairedNoteGroups
          x -= 1
        case (0, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, Set(), annotated(y - 1)) ::
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

    //pairedNoteGroups.foreach { group =>
    //  val PairedNoteGroup(_, estimated, annotated) = group
    //  printf("estimated: %s\n", estimated.map { _.staffY })
    //  printf("annotated: %s\n", annotated)
    //}

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
          values.sortWith(_<_)(values.length / 2)
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

  def drawTemplateMatch(
      _match:TemplateMatch, output:ColorImage, template:GrayImage) {
    val templateScaled = scaleTemplate(template, _match.w, _match.h)
    (0 until templateScaled.h).foreach { templateScaledY =>
      (0 until templateScaled.w).foreach { templateScaledX =>
        val templateV = templateScaled(templateScaledX, templateScaledY)
        val demoX = (_match.x + templateScaledX - templateScaled.w / 2)
        val demoY = (_match.y + templateScaledY - templateScaled.h / 2)
        val (r, g, b) = output(demoX, demoY)
        output(demoX, demoY) = (templateV, g, b)
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
          groups = (oneGroup + point) :: groups.filter { _ != oneGroup }
        case manyGroups =>
          val merged = manyGroups.reduceLeft { _ ++ _ } ++ Set(point)
          groups = merged :: groups.filter { !manyGroups.contains(_) }
      }
    }
    groups
  }

  def listNonOverlappingAlternatives(points:List[TemplateMatch])
      : Set[Set[TemplateMatch]] = {
    points match {
      case Nil => Set(Set[TemplateMatch]())
      case point :: otherPoints =>
        val (overlapping, notOverlapping) = otherPoints.partition { otherPoint=>
          overlapAmount(point, otherPoint) >= 0.3f
        }
        val ifPointKept = listNonOverlappingAlternatives(
            notOverlapping).map { set => set ++ Set(point) }
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
        val multiplier =
          if (v >= otherBlack) 1.0f
          else v.floatValue / otherBlack
        val v2 = v - ((expectedV - white) * multiplier).intValue
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
    val demo4 = new GrayImage(input.w, input.h)
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
        demo4(x, y) = demo(x, y)._3
      }
    }
    demo2.saveTo(new File("demos/erase2.%s.png".format(caseName)))
    demo4.saveTo(new File("demos/erase4.%s.png".format(caseName)))

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

    (demo2, demo4)
  }

  def scanSegments(input:GrayImage) : List[Segment] = {
    var segments:List[Segment] = Nil
    val background = 0
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
            activeGroups = activeGroups.filter { group =>
              group != matchedGroup && !otherGroups.contains(group)
            }
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

  def filterImage(input:GrayImage)(
      filter:(GrayImage,Int,Int)=>Int) : GrayImage = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        output(x, y) = filter(input, x, y)
      }
    }
    output
  }

  def findLeftEdges(input:GrayImage) = {
    filterImage(input) { (input, x, y) =>
      if (input(x - 1, y) - input(x + 1, y) > 50) 255
      else 0
    }
  }

  def findRightEdges(input:GrayImage) = {
    filterImage(input) { (input, x, y) =>
      if (input(x - 1, y) - input(x + 1, y) < -50) 255
      else 0
    }
  }

  def slideTemplate(input:GrayImage, template:GrayImage)(
      scorer:(Int,Int)=>Boolean) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { inputX =>
      (0 until input.h).foreach { inputY =>
        val inputV = input(inputX, inputY)
        (0 until template.w).foreach { templateX =>
          (0 until template.h).foreach { templateY =>
            val templateV = template(templateX, templateY)
            val predictedCenterX = inputX - (templateX - template.w/2)
            val predictedCenterY = inputY - (templateY - template.h/2)
            if (predictedCenterX >= 0 && predictedCenterX < output.w &&
                predictedCenterY >= 0 && predictedCenterY < output.h &&
                scorer(inputV, templateV)) {
              output(predictedCenterX, predictedCenterY) =
                output(predictedCenterX, predictedCenterY) + 1
            }
          }
        }
      }
    }
    output
  }

  def tryTemplateAt(input:GrayImage, template:GrayImage,
      centerX:Int, centerY:Int)(scorer:(Int,Int)=>Boolean) : Float = {
    var score = 0
    (0 until template.w).foreach { templateX =>
      (0 until template.h).foreach { templateY =>
        val inputX = centerX + templateX - template.w/2
        val inputY = centerY + templateY - template.h/2
        val inputV = input(inputX, inputY)
        val templateV = template(templateX, templateY)
        val scoreDelta = (if (scorer(inputV, templateV)) 1 else 0)
        score += scoreDelta
      }
    }
    score / (template.w * template.h).floatValue
  }

  def findDiagonalLines(input:GrayImage, polarity:Boolean) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val diff1 = (input(x, y) - input(x - 2, y - 2))
        val diff2 = (input(x, y) - input(x + 2, y + 2))
        val newV =
          if (polarity && diff1 < 0 && diff2 < 0) -(diff1 + diff2) / 2
          else if (!polarity && diff1 > 0 && diff2 > 0) (diff1 + diff2) / 2
          else 0
        output(x, y) = newV
      }
    }
    output
  }

  def findWhiteHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      caseName:String) = {
    val templateDipsT = findDiagonalLines(rightSizeTemplate, true)
    val templateDipsF = findDiagonalLines(rightSizeTemplate, false)
    templateDipsT.saveTo(new File(
      "demos/find_white.tdt.%s.png".format(caseName)))
    templateDipsF.saveTo(new File(
      "demos/find_white.tdf.%s.png".format(caseName)))

    val inputDipsT = findDiagonalLines(justNotes.inverse, true)
    val inputDipsF = findDiagonalLines(justNotes.inverse, false)
    inputDipsT.saveTo(new File("demos/find_white.idt.%s.png".format(caseName)))
    inputDipsF.saveTo(new File("demos/find_white.idf.%s.png".format(caseName)))

    val trueMatch = slideTemplate(inputDipsT, templateDipsT) {
      (inputV, templateV) =>
      inputV > 20 && templateV > 20
    }
    trueMatch.scaleValueToMax255.saveTo(new File(
      "demos/find_white.t.%s.png".format(caseName)))

    val falseMatch = slideTemplate(inputDipsF, templateDipsF) {
      (inputV, templateV) =>
      inputV > 20 && templateV > 20
    }
    falseMatch.scaleValueToMax255.saveTo(new File(
      "demos/find_white.f.%s.png".format(caseName)))

    val combinedMatch =
      GrayImage.giveBrightnessPerPixel(justNotes.w, justNotes.h) { (x, y) =>
        trueMatch(x, y) * falseMatch(x, y) / 2 // divide by two for image
      }
    combinedMatch.scaleValueToMax255.saveTo(new File(
      "demos/find_white.tf.%s.png".format(caseName)))
    combinedMatch
  }

  def findAccidental(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      augmentedCaseName:String) = {
    val input = justNotes.inverse

    val inputLeftEdges = findLeftEdges(input)
    val templateLeftEdges = findLeftEdges(rightSizeTemplate)
    val leftEdgeMatch = slideTemplate(inputLeftEdges, templateLeftEdges) {
      (inputV, templateV) => inputV == 255 && templateV == 255
    }

    val inputRightEdges = findRightEdges(input)
    val templateRightEdges = findRightEdges(rightSizeTemplate)
    val rightEdgeMatch = slideTemplate(inputRightEdges, templateRightEdges) {
      (inputV, templateV) => inputV == 255 && templateV == 255
    }

    val combinedEdgeMatch = 
        GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      leftEdgeMatch(x, y) + rightEdgeMatch(x, y)
    }
    val combinedEdgeMatch255 = combinedEdgeMatch.scaleValueToMax255

    val isDarkMatch = slideTemplate(input, rightSizeTemplate) {
      (inputV, templateV) => inputV > 128 && templateV == 255
    }
    val isDarkMatch255 = isDarkMatch.scaleValueToMax255

    val demo = GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      val r = combinedEdgeMatch255(x, y)
      val g = 0
      val b = isDarkMatch255(x, y)
      val rb = r * b / 255
      rb
    }
    demo.saveTo(new File(
      "demos/find_accidental.%s.png".format(augmentedCaseName)))

    val output = GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      combinedEdgeMatch(x, y) + isDarkMatch(x, y) / 2
    }
    output
  }

  def makeGradientImages(input:GrayImage, range:Int) : List[GrayImage] = {
    val gradientX     = new GrayImage(input.w, input.h)
    val gradientY     = new GrayImage(input.w, input.h)
    val gradientNorm  = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        var sumX = 0
        var sumY = 0
        var denom = 0
        (-range to range).foreach { xNeighbor =>
          (-range to range).foreach { yNeighbor =>
            val v = input(x + xNeighbor, y + yNeighbor)
            val xSign =
              if (xNeighbor == 0) 0 else (xNeighbor / Math.abs(xNeighbor))
            val ySign =
              if (yNeighbor == 0) 0 else (yNeighbor / Math.abs(yNeighbor))
            sumX += v * xSign
            sumY += v * ySign
            denom += 1
          }
        }
        var meanX = sumX / denom
        var meanY = sumY / denom

        val norm = Math.sqrt(meanX * meanX + meanY * meanY)
        if (norm > 0.0f) {
          gradientX(x, y) = (meanX * 100 / norm).intValue + 100
          gradientY(x, y) = (meanY * 100 / norm).intValue + 100
          gradientNorm(x, y) = norm.intValue
        } else {
          gradientX(x, y) = 127
          gradientY(x, y) = 127
          gradientNorm(x, y) = 0
        }
      }
    }
    List(gradientX, gradientY, gradientNorm)
  }

  def findTrebleClef(
      justNotes:GrayImage, template:GrayImage, caseName:String) = {
    val input = justNotes.inverse

    val List(templateGradientX, templateGradientY, _) =
      makeGradientImages(template.addMargin(4), 3)
    templateGradientX.saveTo(new File(
      "demos/template_gradient_x.%s.png".format(caseName)))
    templateGradientY.saveTo(new File(
      "demos/template_gradient_y.%s.png".format(caseName)))

    val List(inputGradientX, inputGradientY, _) = makeGradientImages(input, 3)
    inputGradientX.saveTo(new File(
      "demos/input_gradient_x.%s.png".format(caseName)))
    inputGradientY.saveTo(new File(
      "demos/input_gradient_y.%s.png".format(caseName)))

    val gradientXResults =
      slideTemplate(inputGradientX, templateGradientX) { (inputV, templateV) =>
        inputV != 127 && templateV != 127 && Math.abs(templateV - inputV) < 2
      }
    val gradientYResults =
      slideTemplate(inputGradientY, templateGradientY) { (inputV, templateV) =>
        inputV != 127 && templateV != 127 && Math.abs(templateV - inputV) < 2
      }

    val hough255X = gradientXResults.scaleValueToMax255
    val hough255Y = gradientYResults.scaleValueToMax255
    hough255X.saveTo(new File("demos/treble_match_x.%s.png".format(caseName)))
    hough255Y.saveTo(new File("demos/treble_match_y.%s.png".format(caseName)))

    val output = GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      gradientXResults(x, y) * gradientYResults(x, y) / 64
    }
    output
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

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def readFile(f: java.io.File) = {
    val source = scala.io.Source.fromFile(f)
    val contents = source.mkString
    source.close()
    contents
  }

  def saveMetrics(metrics:Metrics, file:File) {
    val asMap = Map(
      "w" -> metrics.w,
      "h" -> metrics.h,
      "a" -> metrics.a,
      "b" -> metrics.b,
      "c" -> metrics.c,
      "cSpacing" -> metrics.cSpacing,
      "bSpacing" -> metrics.bSpacing
    )
    val out = Json.build(asMap).toString()
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def restoreMetrics(file:File) = {
    val inString = readFile(file)
    val json:Map[String,scala.math.BigDecimal] = 
      Json.parse(inString).asInstanceOf[Map[String,scala.math.BigDecimal]]
    Metrics(
      json("w").toFloat,
      json("h").toFloat,
      json("a").toFloat,
      json("b").toFloat,
      json("c").toFloat,
      json("cSpacing").toFloat,
      json("bSpacing").toFloat
    )
  }

  def saveGrayImage(image:GrayImage, file:File) {
    image.saveTo(file)
  }

  def loadGrayImage(file:File) = {
    ColorImage.readFromFile(file).toGrayImage.inverse
  }

  def readOrGenerate[T](file:File, save:((T,File)=>Unit), restore:(File=>T))(
      generator:()=>T) = {
    if (!file.exists()) {
      printf("Generating %s...\n", file.getPath())
      val output = generator()
      save(output, file)
    }
    restore(file)
  }

  def findBlackHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      possiblePoints:List[(Int,Int,Int)], caseName:String) : List[Float] = {
    /*val leftEdge = Array(-1, 0, 1, -2, 0, 2, -1, 0, 1, 4)
    val rightEdge = Array(1, 0, -1, 2, 0, -2, 1, 0, -1, 4)
    val topEdge = Array(-1, -2, -1, 0, 0, 0, 1, 2, 1, 4)
    val bottomEdge = Array(1, 2, 1, 0, 0, 0, -1, -2, -1, 4)
    val blur = Array(1, 2, 1, 2, 4, 2, 1, 2, 1, 16)
    var guessFromEdge = new GrayImage(0, 0)
    //List(leftEdge, rightEdge, topEdge, bottomEdge, blur).foreach { edge =>
    List(leftEdge).foreach { edge =>
      val threshold = 50 // should be 250 for blur?
      val templateEdge =
        edgeDetection(rightSizeTemplate, edge).binarize(threshold)
      //templateEdge.saveTo(new File(
      //  "demos/smalltemplate.l.%s.png".format(caseName)))
      val inputEdge =
        edgeDetection(justNotes.inverse, edge).binarize(threshold)
      //inputEdge.saveTo(new File(
      //  "demos/edgedetect.%s.png".format(caseName)))
      guessFromEdge = slideTemplate(inputEdge, templateEdge) {
        (inputV, templateV) => inputV == 255 && templateV == 255
      }
    }*/

    val threshold = 128
    val templateDistance = distance(rightSizeTemplate, threshold)
    //templateDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.t.%s.png".format(caseName)))
    val inputDistance = distance(justNotes, threshold)
    //inputDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.i.%s.png".format(caseName)))

    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      var maxScore = 0.0f
      var argmaxCenterX = 0
      var argmaxCenterY = 0
      (-1 to 1).foreach { xAdjust =>
        (-1 to 1).foreach { yAdjust =>
          val score = tryTemplateAt(inputDistance, templateDistance,
              centerX + xAdjust, centerY + yAdjust) {
            (inputV, templateV) =>
            inputV > 0 && templateV > 0 && inputV - templateV >= 0
          }
          if (score > maxScore) {
            maxScore = score
            argmaxCenterX = centerX + xAdjust
            argmaxCenterY = centerY + yAdjust
          }
        }
      }
      maxScore
    }
    scores
  }

  def gleanPoints(detected:GrayImage, metrics:Metrics,
      yCorrection:Array[Float], templateW:Int, templateH:Int,
      threshold:Int, templateName:String) = {
    //val simplified = image.copy
    var points:List[TemplateMatch] = Nil
    (-8 to 8).foreach { staffY =>
      var maxV = 0
      var argmaxX = 0
      var argmaxY = 0
      var argmaxStaffY = 0
      (0 until detected.w).foreach { x =>
        val xCentered = x - (detected.w / 2)
        val a = metrics.a
        val staffY0 = staffY - 0.5f
        val staffY1 = staffY + 0.5f
        val b0 = metrics.b + (staffY0 / 2.0f * metrics.bSpacing)
        val b1 = metrics.b + (staffY1 / 2.0f * metrics.bSpacing)
        val c0 = metrics.c + (staffY0 / 2.0f * metrics.cSpacing)
        val c1 = metrics.c + (staffY1 / 2.0f * metrics.cSpacing)
        val y0 = Math.round((a * xCentered * xCentered + b0 * xCentered +
          c0) + yCorrection(x) + (detected.h / 2)).intValue
        val y1 = Math.round((a * xCentered * xCentered + b1 * xCentered +
          c1) + yCorrection(x) + (detected.h / 2)).intValue

        var hasPointAboveThreshold = false
//          (y0 until y1).foreach { y =>
val y = (y0 + y1) / 2
          val v = detected(x, y)
          if (v > threshold) {
            hasPointAboveThreshold = true
            if (v > maxV) {
              maxV = v
              argmaxX = x
              //argmaxY = y
              argmaxY = (y0 + y1) / 2
              argmaxStaffY = staffY
            }
          }
//          }
        if (!hasPointAboveThreshold && maxV != 0) {
          //simplified(argmaxX, argmaxY) = 255
          points = TemplateMatch(argmaxX, argmaxY, templateW, templateH,
            argmaxStaffY, templateName) :: points
          maxV = 0
        }

        //if (y0 >= 0 && y0 < simplified.h)
        //  simplified(x, y0) = 0
      }
    }
    points
  }

  def chooseBestOverlappingSets(
      bounds:BoundingBox,
      overlappingPointGroup:List[TemplateMatch],
      templates:Map[String,GrayImage], orthonormalImage:GrayImage) :
      Set[TemplateMatch] = {
    val alternatives = listNonOverlappingAlternatives(overlappingPointGroup)

    var bestAlternative = alternatives.toList(0)
    var bestI = 0
    var maxScore = -999999
    var i = 0
    alternatives.foreach { pointGroup =>
      var proposal = new ColorImage(bounds.maxX - bounds.minX + 1,
                                   bounds.maxY - bounds.minY + 1)
      (0 until proposal.w).foreach { x =>
        (0 until proposal.h).foreach { y =>
          var b = orthonormalImage(x + bounds.minX, y + bounds.minY)
          proposal(x, y) = (0, 0, b)

        }
      }
      pointGroup.foreach { point =>
        val template = templates(point.templateName)
        (0 until template.h).foreach { templateY =>
          (0 until template.w).foreach { templateX =>
            val x = (point.x - point.w/2) + templateX - bounds.minX
            val y = (point.y - point.h/2) + templateY - bounds.minY
            val (r, g, b) = proposal(x, y)
            proposal(x, y) = (r max template(templateX, templateY), g, b)
          }
        }
      }
      //proposal.saveTo(new File(
      //  "demos/proposal.%03d.%02d.png".format(bounds.minX, i)))

      var diff = 0
      (0 until proposal.w).foreach { x =>
        (0 until proposal.h).foreach { y =>
          diff += Math.abs(proposal(x, y)._1 -
            orthonormalImage(x + bounds.minX, y + bounds.minY))
        }
      }
      val score = -diff
        
      if (score > maxScore) {
        maxScore = score
        bestAlternative = pointGroup
        bestI = i
      }

      i += 1
    }
    //println("best for %03d is %d".format(bounds.minX, bestI))
    bestAlternative
  }

  def excerptComparison(x:Int, y:Int,
      justNotes:GrayImage, templates:Map[String,GrayImage], caseName:String) {
    val margin = 8
    val threshold = 128
  
    val inputExcerpt = distance(justNotes.inverse.crop
      (x - margin, y - margin, margin * 2, margin * 2), threshold)
  
    val templateSmall = templates("black_head")
    val templateWhitespace = new GrayImage(margin * 2, margin * 2)
    (0 until templateSmall.h).foreach { y =>
      (0 until templateSmall.w).foreach { x =>
        templateWhitespace(
          x - templateSmall.w/2 + margin - 1,
          y - templateSmall.h/2 + margin) = templateSmall(x, y)
      }
    }
    val templateExcerpt = distance(templateWhitespace, threshold)
  
    val inputExcerpt255 = inputExcerpt.scaleValueToMax255
    val templateExcerpt255 = templateExcerpt.scaleValueToMax255
    val demo = new ColorImage(margin * 2, margin * 8)
    (0 until inputExcerpt.h).foreach { y =>
      (0 until inputExcerpt.w).foreach { x =>
        val v = inputExcerpt255(x, y)
        demo(x, y) = (v, v, v)
        demo(x, y + margin * 4) = (v, 0, 0)
      }
    }
    (0 until templateExcerpt.h).foreach { y =>
      (0 until templateExcerpt.w).foreach { x =>
        val v = templateExcerpt255(x, y)
        demo(x, y + margin * 2) = (v, v, v)
  
        val (r, _, _) = demo(x, y + margin*4)
        demo(x, y + margin * 4) = (r, 0, v)
  
        val inputV = inputExcerpt(x, y)
        val templateV = templateExcerpt(x, y)
        val match_ = inputV > 0 && templateV > 0 &&
          inputV - templateV >= -1 && inputV - templateV <= 0
        demo(x, y + margin * 6) = (0, if (match_) 255 else 0, 0)
      }
    }
    demo.saveTo(new File("demos/compare.black.%s.png".format(caseName)))
  }
  //excerptComparison(425,26) // on 3h

  def findVerticalLines(input:GrayImage) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        val sum = (
          input(x - 4, y) * -4 +
          input(x - 3, y) * -2 +
          input(x - 2, y) *  0 +
          input(x - 1, y) *  1 +
          input(x + 0, y) *  2 +
          input(x + 1, y) *  1 +
          input(x + 2, y) *  0 +
          input(x + 3, y) * -2 +
          input(x + 4, y) * -4 +
          0) / 12
        output(x, y) = sum
      }
    }
    output
  }

  // Vertical lines (e.g. stems, measure lines, etc.) are often slightly
  // diagonal.  Traditional y/x slope acts weird around vertical lines,
  // so we're using x/y slope instead.
  // This method reports 1) the slope of the stem lines at the far left
  // of the page, as well as 2) the slope of the stem lines at the far
  // right of the page.  It's assumed that the slope of any vertical lines
  // between those points can be determined with linear interpolation.
  // So you can consider these two values as a line on the Hough transform
  // from (xIntercept=far left, slope=y1) to (xIntercept=far right, slope=y2).
  // (A line on the Hough transform corresponds to a series of nearly parallel
  // lines on the image).
  def findVLineInverseSlopeRangeGivenHough(
      vhough:GrayImage, image:GrayImage, threshold:Int, caseName:String) = {

    // Brute force linear regression: use a threshold on the Hough output
    // to find just the bright spots.  Then try all possible lines
    // with O(n^2) algorithm to keep the line with the brightest points on it.

    // Special cases:
    // 0) If no Hough bright spots pass the threshold, meaning that the Hough
    // transform couldn't find any long enough lines, then return the default
    // (reporting that lines are perfectly vertical).  Not too accurate,
    // but it's better than erroneously reporting very diagonal lines.

    // 1) If only one Hough bright spot passes the threshold, meaning that
    // there was one long line found in the image, but no other lines reliable
    // enough to determine how the perspective should vary the lines from the
    // left to the right of the image, then report a horizontal line (on
    // the Hough transform) through that bright spot (in other words, all 
    // near-vertical lines are parallel to the long one).

    var maxSum = 0
    var argmaxYLeft = vhough.h / 2 // default
    var argmaxYRight = vhough.h / 2 // default
    (0 until vhough.h).foreach { yLeft =>
      // Constrain yRight >= yLeft, because we assume that the stem lines
      // on the right side of the page are more counter-clockwise bent
      // than stem lines on the left side of the page.  Like this:  ///|||\\\
      // That should be the case if the camera is above and behind the paper.
      // Taking pictures from below or in front of the paper tricky to do
      // because of gravity and shadows, respectively.  If you could,
      // then stems lines would bend like this:  \\\|||/// and yRight should
      // be allowed to be < yLeft.
      (yLeft until vhough.h).foreach { yRight =>
        var sum = 0
        (0 until vhough.w).foreach { x =>
          val y = yLeft + (yRight - yLeft) * x / vhough.w
          val v = vhough(x, y)
          if (v > threshold)
            sum += (v - threshold)
          // the reason to subtract before adding to sum is that otherwise
          // the argmax line prefers to go through the Hough line where it's
          // thickest (the most points), not just where it's brightest
        }
    
        // Use >= instead of > so that if many lines have the same sum,
        // we'll pick the latest one, which happens to be the most horizontal,
        // since both yLeft and yRight will be maximized to pass through
        // whatever the bright point is.
        // The sum > 0 check is so the default doesn't get overridden
        if (sum > 0 && sum >= maxSum) {
          maxSum = sum
          argmaxYLeft = yLeft
          argmaxYRight = yRight
        }
      }
    }
    
    /*val demo = vhough.scaleValueToMax255.toColorImage
    (0 until demo.w).foreach { x =>
      val y = argmaxYLeft + (argmaxYRight - argmaxYLeft) * x / vhough.w
      val (r, g, b) = demo(x, y)
      demo(x, y) = (63 max r, g, b)
    }
    demo.saveTo(new File("demos/vhoughnew.%s.png".format(caseName)))
    
    var vlines:List[(Int,Int)] = Nil
    (0 until vhough.w).foreach { x =>
      val y = argmaxYLeft + (argmaxYRight - argmaxYLeft) * x / vhough.w
      if (vhough(x, y) > 2)
        vlines = (x, y) :: vlines
    }
    
    val demo2 = image.toColorImage
    vlines.foreach { vline =>
      val (xIntercept, inverseSlope40) = vline
      (0 until demo2.h).foreach { y =>
        val x =
          xIntercept + Math.round((inverseSlope40 - 20) / 80.f * y).intValue
        val (r, g, b) = demo2(x, y)
        demo2(x, y) = ((r + 63) min 255, g, b)
      }
    }
    demo2.saveTo(new File("demos/vlinesshown.%s.png".format(caseName)))*/

    (argmaxYLeft, argmaxYRight)
  }
    
  def findThickHorizontalLines(
      justNotes:GrayImage, metrics:Metrics, caseName:String) = {
    val topEdges = edgeDetection(
      justNotes, Array(1, 2, 1, 0, 0, 0, -1, -2, -1, 4))
    val topEdgesBlurred = edgeDetection(
      topEdges, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(30)
    val bottomEdges = edgeDetection(
      justNotes, Array(-1, -2, -1, 0, 0, 0, 1, 2, 1, 4))
    val bottomEdgesBlurred = edgeDetection(
      bottomEdges, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(30)
    //val blurred = edgeDetection(
    //  justNotes.inverse, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(200)

    val thickLines = new ColorImage(justNotes.w, justNotes.h)
    val multiplier = 0.8f
    (0 until justNotes.w).foreach { x =>
      val xCentered = x - justNotes.w/2
      val staffSeparation = (metrics.bSpacing * xCentered) + metrics.cSpacing
      val expectedBeamWidth =
        Math.round(staffSeparation * multiplier).intValue - 2
      (0 until justNotes.h).foreach { y =>
        val r = topEdgesBlurred(x, y - expectedBeamWidth / 2)
        val g = bottomEdgesBlurred(x, y + (expectedBeamWidth + 1) / 2)
        //val b = blurred(x, y)
        thickLines(x, y) = (r, g, 0)
      }
    }
    thickLines.saveTo(new File("demos/beamlike.%s.png".format(caseName)))
    thickLines
  }

  def findBeams(thickLines:ColorImage, image:GrayImage, caseName:String) = {
    val minBeamLength = 25
    val threshold = 23
    val demo = image.toColorImage
    var beams:List[Beam] = Nil
    (-4 to 4).foreach { slopeTenths =>
      val y0 = (0 - image.w * slopeTenths / 10) min 0
      val y1 = (image.w + image.w * slopeTenths / 10) max (image.w - 1)
      (y0 to y1).foreach { startingY =>
        var numBlacksInWindow = 0
        (0 until image.w).foreach { x =>
          val y = startingY + (x * slopeTenths / 10)
          if (thickLines(x, y) == (255, 255, 0))
            numBlacksInWindow += 1
    
          val windowX = x - minBeamLength
          val windowY = startingY + (windowX * slopeTenths / 10)
          if (thickLines(windowX, windowY) == (255, 255, 0))
            numBlacksInWindow -= 1
    
          if (numBlacksInWindow >= threshold) {
            var foundBeam = false
            val newX0 = x - threshold
            val newX1 = x - 1
            val newY0 = startingY + (newX0 * slopeTenths / 10)
            val newY1 = y
            beams.foreach { box =>
              if (!foundBeam && newX0 <= box.x1 + 5 && newX1 >= box.x0 - 5) {
                if (newY0 <= box.y1 + 2 && newY1 >= box.y0 - 2) {
                  val newBeam = Beam(box.x0 min newX0, box.x1 max newX1,
                                   box.y0 min newY0, box.y1 max newY1)
                  beams = newBeam :: beams.filter { _ != box }
                  foundBeam = true
                }
                else if (newY1 <= box.y0 + 2 && newY0 >= box.y1 - 2) {
                  val newBeam = Beam(box.x0 min newX0, box.x1 max newX1,
                                   box.y0 max newY0, box.y1 min newY1)
                  beams = newBeam :: beams.filter { _ != box }
                  foundBeam = true
                }
              }
            }
            if (!foundBeam) {
              val newBeam = Beam(newX0, newX1, newY0, newY1)
              beams = newBeam :: beams
            }
    
            (windowX to x).foreach { xBefore =>
              demo(xBefore, y - (x - xBefore) * slopeTenths / 10) = (255, 0, 0)
            }
          }
        }
      }
    }
    demo.saveTo(new File("demos/beams.%s.png".format(caseName)))
    beams
  }

  def demoBeams(beams:List[Beam], image:GrayImage, caseName:String) {
    val demo2 = image.toColorImage
    val red = (255, 0, 0)
    beams.foreach { beam =>
      (beam.x0 to beam.x1).foreach { x =>
        val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
        val y = beam.y0 + ((beam.y1 - beam.y0) * progress).intValue
        demo2(x, y) = red
      }
    }
    demo2.saveTo(new File("demos/beamboxes.%s.png".format(caseName)))
  }

  def minXOfShape(shape:List[Segment]) : Int = {
    shape.foldLeft(999999) { _ min _.x0 }
  }

  def maxXOfShape(shape:List[Segment]) : Int = {
    shape.foldLeft(-999999) { _ max _.x1 }
  }

  // two shapes are "x-overlapping" if there's no vertical line
  // that separates them without intersecting either shape
  def groupXOverlappingShapes(shapes:List[List[Segment]]) :
      List[List[List[Segment]]] = {
    shapes match {
      case Nil => Nil
      case _ =>
        var minXOfAllShapes = shapes.foldLeft(999999) { _ min minXOfShape(_) }
        var argminShape = shapes.find { minXOfShape(_) == minXOfAllShapes }.get
        var foundSeparation = false
        var separationX = maxXOfShape(argminShape)
        while (!foundSeparation) {
          var (nonOverlappingShapes, overlappingShapes) =
            shapes.partition { minXOfShape(_) > separationX }
          foundSeparation = true
          overlappingShapes.foreach { overlappingShape =>
            val newMaxX:Int = maxXOfShape(overlappingShape)
            if (newMaxX > separationX) {
              separationX = newMaxX
              foundSeparation = false
            }
          }
        }
        var (nonOverlappingShapes, overlappingShapes) =
          shapes.partition { minXOfShape(_) > separationX }
        overlappingShapes :: groupXOverlappingShapes(nonOverlappingShapes)
    }
  }

  def eraseBeams(input:GrayImage, beams:List[Beam], metrics:Metrics) = {
    val output = input.copy
    val multiplier = 2.0f // especially thick because of surrounding gray pixels
    beams.foreach { beam =>
      (beam.x0 to beam.x1).foreach { x =>
        val xCentered = x - input.w/2
        val staffSeparation = (metrics.bSpacing * xCentered) + metrics.cSpacing
        val expectedBeamWidth =
          Math.round(staffSeparation * multiplier).intValue - 2
        val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
        val yMid = (beam.y0 + (beam.y1 - beam.y0) * progress).intValue
        val yTop = (yMid - expectedBeamWidth/2) max 0
        val yBottom = (yMid + (expectedBeamWidth+1)/2) min (input.h - 1)
        (yTop to yBottom).foreach { y =>
          output(x, y) = 0
        }
      }
    }
    output
  }

  def verticalHough(input:ColorImage, caseName:String) = {
    val hough = new GrayImage(input.w, 40)
    (0 until input.h).foreach { inputY =>
      (10 until input.w - 10).foreach { inputX => // avoid edges
        val v = input(inputX, inputY)
        (-20 until 20).foreach { mCents =>
          val inputXIntercept =
            Math.round(inputX - (mCents / 80.0f * inputY)).intValue
          if (inputXIntercept >= 0 && inputXIntercept < hough.w) {
            if (v == (0, 0, 127)) // negative spot
              hough(inputXIntercept, mCents + 20) =
                hough(inputXIntercept, mCents + 20) - 3
            else
              hough(inputXIntercept, mCents + 20) =
                hough(inputXIntercept, mCents + 20) + (v._1 / 63)
          }
        }
      }
    }
    
    (0 until hough.h).foreach { y =>
      (0 until hough.w).foreach { x =>
        hough(x, y) = (hough(x, y) - 40) max 0
      }
    }

    hough.scaleValueToMax255.saveTo(new File(
      "demos/vhough2.%s.png".format(caseName)))
    hough
  }

  def findVLineInverseSlopeRange(
      justNotes2:GrayImage, image:GrayImage, caseName:String) = {
    val justNotes2Distance = distance(justNotes2, 200)
    val justNotes2Blurred = edgeDetection(
      justNotes2, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(20)
    val demo = ColorImage.giveRGBPerPixel(image.w, image.h) { (x, y) =>
      if (justNotes2Distance(x, y) >= 3)
        (0, 0, 127)
      else {
        var v = justNotes2Blurred(x, y) / 4
        (v, v, v)
      }
    }
    val vhough = verticalHough(demo, caseName)
    val (atLeft, atRight) =
      findVLineInverseSlopeRangeGivenHough(vhough, image, 0, caseName)
    demo.saveTo(new File("demos/stems.%s.png".format(caseName)))
    justNotes2Blurred.saveTo(new File("demos/blurred.%s.png".format(caseName)))

    ((atLeft - 20) / 80.0f, (atRight - 20) / 80.0f)
  }

  // now that you know where to expect lines, do a better job of finding them
  def doVLineDetection(justNotes2:GrayImage, image:GrayImage,
      inverseSlopeRange:(Float,Float), caseName:String) = {
    val vEdges = findVerticalLines(justNotes2).binarize(10)
    vEdges.saveTo(new File("demos/vedges.%s.png".format(caseName)))

    val justNotes2Blurred = edgeDetection(
      justNotes2, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(20)

    var vlines:List[VLine] = Nil
    val demo3 = image.toColorImage
    val (inverseSlopeAtLeft, inverseSlopeAtRight) = inverseSlopeRange
    (0 until image.w).foreach { xIntercept =>
      val progress = xIntercept / image.w.floatValue
      val inverseSlope = inverseSlopeAtLeft +
        (inverseSlopeAtRight - inverseSlopeAtLeft) * progress

      var sum = 0
      (0 until image.h).foreach { y =>
        val x = xIntercept + (inverseSlope * y).intValue
        val v = vEdges(x, y) / 255
        sum += v
      }

      val valuesSum = new Array[Int](image.h) // like a summed-area table in 1D
      var sum2 = 0
      (0 until image.h).foreach { y =>
        val x = xIntercept + (inverseSlope * y).intValue
        val v = justNotes2Blurred(x, y) / 255
        sum2 += v
        valuesSum(y) = sum2
      }

      var maxScore = 0
      var argmaxY0 = 0
      var argmaxY1 = 0
      (0 until (image.h - 1)).foreach { y0 =>
        ((y0 + 10) until image.h).foreach { y1 =>
          var sum = valuesSum(y1) - valuesSum(y0)
          val fullness = sum / (y1 - y0).floatValue
          val score = if (fullness > 0.99f && y1 - y0 >= 30) (y1 - y0) else 0
          if (score > maxScore) {
            maxScore = score
            argmaxY0 = y0
            argmaxY1 = y1
          }
        }
      }

      if (sum > 5) {
        //(0 until image.h).foreach { y =>
        (argmaxY0 until argmaxY1).foreach { y =>
          val x = xIntercept + (inverseSlope * y).intValue
          val (r, g, b) = demo3(x, y)
          val rNew = (r + 50) min 255
          demo3(x, y) = (rNew, g, b)
        }
        vlines = VLine(xIntercept, argmaxY0, argmaxY1) :: vlines
      }
    }
    demo3.saveTo(new File("demos/stems2.%s.png".format(caseName)))

    vlines
  }

  // by "orthonormal" I mean that the vertical lines in the image are pointing
  // straight up and down, and the horizontal staff lines are pointing
  // straight left and right, instead of both being somewhat diagonal.
  // It doesn't mean that the x and y vectors are unit length.
  def orthonormalize(input:GrayImage,
      vLineSlopeRange:(Float,Float), metrics:Metrics, yCorrection:Array[Float],
      caseName:String) : Orthonormal = {
    val (b0, b1) = vLineSlopeRange

    def targetYFor(xUncentered:Int, yUncentered:Int) = {
      val xCentered = xUncentered - input.w/2
      val yCentered = yUncentered - input.h/2

      // TODO poorly chosen notation:
      //   b0 below means something different than b0 above
      // Equation for y-skewing: (y stands for staffY)
      //   y' = a0*x*x + (b0 + b*y)*x + (c0 + c*y) + correct(x)
      // Solve for y (staffY):
      //   y' - a0*x*x - b0*x - c0 - correct(x) = b*y/2*x + c*y/2
      //   y = (y' - a0*x*x - b0*x - c0 - correct(x)) / (b*x + c) * 2
      val staffY = (yCentered -
        metrics.a * xCentered * xCentered -
        metrics.b * xCentered - metrics.c - yCorrection(xUncentered)) /
        (metrics.bSpacing * xCentered + metrics.cSpacing) * 2.0f

      // We don't want to use staffY for the transformed input, because it
      // has too few pixels per staff line; instead we want staffY*c,
      // which has one target pixel per source pixel in the center, and
      // slightly more/less than that at the edges
      Math.round(staffY/2.0f * metrics.cSpacing + input.h/2).intValue
    }
    val minTargetY = (0 until input.w).map { targetYFor(_, 0) }.min
    val maxTargetY = (0 until input.w).map { targetYFor(_, input.h - 1) }.max

    def targetXFor(sourceX:Int, sourceY:Int) = {
      // Equation for x-skewing:
      //   x' = x0 + (b0 + (x0/w)*(b1-b0)) * y
      // Solve for x0 given x':
      //   x' = x0 + b0*y + x0*(b1-b0)*y/w
      //   x' - b0*y = x0 + x0*(b1-b0)*y/w
      //   x0 * (1 + (b1-b0)*y/w) = x' - b0*y
      //   x0 = (x' - b0*y) / (1 + (b1-b0)*y/w)
      val targetX =
        (sourceX - b0 * sourceY) / (1.0f + (b1-b0) * sourceY / input.w)

      // Although we don't model x-scaling or how it changes from left to
      // right, it should correlate with the y-scaling
      val xCentered = sourceX - input.w/2
      val staffSeparation = (xCentered * metrics.bSpacing) + metrics.cSpacing
      val proportion = staffSeparation / metrics.cSpacing
      Math.round(targetX / proportion).intValue
    }
    val minTargetX =
      targetXFor(0, 0) min targetXFor(0, input.h - 1)
    val maxTargetX =
      targetXFor(input.w - 1, 0) max targetXFor(input.w - 1, input.h - 1)

    val squaredUp = new GrayImage(maxTargetX - minTargetX + 1 + 1,
                                  maxTargetY - minTargetY + 1 + 1)
    (0 until input.w).foreach { sourceX =>
      (0 until input.h).foreach { sourceY =>
        val v = input(sourceX, sourceY)

        val targetX = targetXFor(sourceX, sourceY) - minTargetX
        val targetY = targetYFor(sourceX, sourceY) - minTargetY

        squaredUp(targetX,     targetY)     = v
        squaredUp(targetX + 1, targetY)     = v
        squaredUp(targetX,     targetY + 1) = v
        squaredUp(targetX + 1, targetY + 1) = v
      }
    }
    squaredUp.saveTo(new File("demos/squared.%s.png".format(caseName)))

    def yForStaffY(staffY:Int) = {
      val y = Math.round(staffY/2.0f * metrics.cSpacing + input.h/2).intValue
      y - minTargetY
    }

    val xForXIntercept = (0 until input.w).map {
      targetXFor(_, 0) - minTargetX
    }.toArray
    val yForStaffYMap = (-8 to 8).foldLeft(Map[Int,Int]()) { (map, staffY) =>
      map.updated(staffY, yForStaffY(staffY))
    }
    val transformXY = { (x:Int, y:Int) =>
      (targetXFor(x, y) - minTargetX, targetYFor(x, y) - minTargetY)
    }

    Orthonormal(squaredUp, yForStaffYMap, xForXIntercept, metrics.cSpacing,
      transformXY)
  }

  def findEasyVerticalCuts(outer:BoundingBox, input:GrayImage,
      ledgerLines:Orthonormal) = {
    val maxVs = new Array[Int](outer.maxX + 1)
    val ledgerLineYs:List[Int] = List(8, 6, -6, -8).map { staffY =>
      ledgerLines.yForStaffY(staffY)
    }
    (outer.minX to outer.maxX).foreach { x =>
      var maxV = 0
      (outer.minY to outer.maxY).foreach { y =>
        val v = input(x, y)
        val onLedgerLine = ledgerLineYs.exists { y2 => Math.abs(y - y2) <= 2 }
        if (v > maxV && !onLedgerLine) {
          maxV = v
        }
      }
      maxVs(x) = maxV
    }

    val isFull = new Array[Boolean](outer.maxX + 1)
    (outer.minX to outer.maxX).foreach { x =>
      isFull(x) = false
    }

    val upperThreshold = 200
    val lowerThreshold = 128
    var isDescending = false
    (outer.minX to outer.maxX).foreach { x =>
      if (maxVs(x) >= upperThreshold)
        isDescending = true
      if (maxVs(x) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(x) = true
    }
    (outer.maxX to outer.minX by -1).foreach { x =>
      if (maxVs(x) >= upperThreshold)
        isDescending = true
      if (maxVs(x) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(x) = true
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartX = outer.minX
    (outer.minX to outer.maxX + 1).foreach { x =>
      val prevFull = (if (x > outer.minX) isFull(x - 1) else false)
      val thisFull = (if (x < outer.maxX) isFull(x) else false)
      if (!prevFull && thisFull)
        boxStartX = x
      else if (prevFull && !thisFull)
        boxes = BoundingBox(boxStartX, x - 1, outer.minY, outer.maxY) :: boxes
    }
    boxes
  }

  def findEasyHorizontalCuts(outer:BoundingBox, input:GrayImage) = {
    val maxVs = new Array[Int](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      var maxV = 0
      (outer.minX to outer.maxX).foreach { x =>
        val v = input(x, y)
        if (v > maxV) {
          maxV = v
        }
      }
      maxVs(y) = maxV
    }

    val isFull = new Array[Boolean](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      isFull(y) = false
    }

    val upperThreshold = 128
    val lowerThreshold = 32
    var isDescending = false
    (outer.minY to outer.maxY).foreach { y =>
      if (maxVs(y) >= upperThreshold)
        isDescending = true
      if (maxVs(y) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(y) = true
    }
    (outer.maxY to outer.minY by -1).foreach { y =>
      if (maxVs(y) >= upperThreshold)
        isDescending = true
      if (maxVs(y) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(y) = true
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartY = outer.minY
    (outer.minY to outer.maxY + 1).foreach { y =>
      val prevFull = (if (y > outer.minY) isFull(y - 1) else false)
      val thisFull = (if (y < outer.maxY) isFull(y) else false)
      if (!prevFull && thisFull)
        boxStartY = y
      else if (prevFull && !thisFull)
        boxes = BoundingBox(outer.minX, outer.maxX, boxStartY, y - 1) :: boxes
    }
    boxes
  }

  def findHorizontalCuts(outer:BoundingBox, input:GrayImage) = {
    val isFull = new Array[Boolean](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      isFull(y) = false
    }

    (outer.minY to outer.maxY).foreach { y =>
      val values = new Array[Int](outer.maxX + 1)
      (outer.minX to outer.maxX).foreach { x =>
        val v = input(x, y)
        values(x) = v
      }
      Sorting.quickSort(values)
      if (values(values.size - 1) >= 255)
        isFull(y) = true
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartY = outer.minY
    (outer.minY to outer.maxY + 1).foreach { y =>
      val prevFull = (if (y > outer.minY) isFull(y - 1) else false)
      val thisFull = (if (y < outer.maxY) isFull(y) else false)
      if (!prevFull && thisFull)
        boxStartY = y
      else if (prevFull && !thisFull)
        boxes = BoundingBox(outer.minX, outer.maxX, boxStartY, y - 1) :: boxes
    }
    boxes
  }

  def cutOnVLines(
      boxes:List[BoundingBox], vlineXs:List[Int]) : List[BoundingBox] = {
    vlineXs match {
      case Nil =>
        boxes
      case x :: otherVLineXs =>
        val newBoxes = boxes.map { box =>
          if (x > box.minX && x < box.maxX) {
            List(BoundingBox(box.minX, x, box.minY, box.maxY),
                 BoundingBox(x, box.maxX, box.minY, box.maxY))
          } else {
            List(box)
          }
        }.foldLeft(List[BoundingBox]()) { _ ++ _ }
        cutOnVLines(newBoxes, otherVLineXs)
    }
  }

  def mergeAdjacent(points:List[Int]) = {
    var newPoints:List[Int] = Nil
    var inRun = false
    var runStart = 0
    var lastPoint = -999
    points.sorted.foreach { x =>
      if (inRun) {
        if (x - lastPoint >= 0 && x - lastPoint <= 1) {
          // do nothing
        } else {
          newPoints = (runStart + lastPoint) / 2 :: newPoints
          inRun = false
        }
      } else {
        if (x - lastPoint >= 0 && x - lastPoint <= 1) {
          inRun = true
          runStart = lastPoint
        } else {
          newPoints = x :: newPoints
        }
      }
      lastPoint = x
    }
    if (inRun) {
      newPoints = (runStart + lastPoint) / 2 :: newPoints
    }
    newPoints
  }

  def doWidthDetection(
      orthonormal:Orthonormal, vlines:List[VLine], caseName:String) = {
    val (minY, maxY) = findYBounds(orthonormal, caseName)
    val input = orthonormal.image
    val wholeImage = BoundingBox(0, input.w - 1, minY, maxY)

    //val horizontalSlices = findHorizontalCuts(wholeImage, input)
    val verticalSlices = findEasyVerticalCuts(wholeImage, input, orthonormal)
    val vlineXs = vlines.map { vline =>
      orthonormal.xForXIntercept(vline.xIntercept) }
    //val boxes = cutOnVLines(verticalSlices, mergeAdjacent(vlineXs))

    val demo = input.toColorImage
    val color = (255, 0, 0)
    def draw(x:Int, y:Int) {
      val (r, g, b) = demo(x, y)
      val rNew = (r + color._1) min 255
      val gNew = (g + color._2) min 255
      val bNew = (b + color._3) min 255
      demo(x, y) = (rNew, gNew, bNew)
    }
    verticalSlices.foreach { box =>
      (box.minX to box.maxX).foreach { x =>
        draw(x, box.minY)
        draw(x, box.maxY)
      }
      (box.minY to box.maxY).foreach { y =>
        draw(box.minX, y)
        draw(box.maxX, y)
      }
    }
    demo.saveTo(new File("demos/segments.%s.png".format(caseName)))
    verticalSlices

/*

    val demo = justNotes2.toColorImage
    (0 until image.w).foreach { xIntercept =>
      val progress = xIntercept / image.w.floatValue
      val inverseSlope =
        vLineSlopeAtLeft + (vLineSlopeAtRight - vLineSlopeAtLeft) * progress

      val (rAdjust, gAdjust, bAdjust) = color(xIntercept)
      (0 until image.h).foreach { y =>
        val x = xIntercept + Math.round(inverseSlope * y).intValue
        val (r, g, b) = demo(x, y)
        val rNew = r max rAdjust
        val gNew = g max gAdjust
        val bNew = b max bAdjust
        demo(x, y) = (rNew, gNew, bNew)
      }
    }
    demo.saveTo(new File("demos/widths.%s.png".format(caseName)))
*/
  }

  def findYBounds(orthonormal:Orthonormal, caseName:String) = {
    val text = orthonormal.image.toColorImage
    var cutoff = new Array[Int](orthonormal.image.w)
    val staffY6 = orthonormal.yForStaffY(6)
    (0 until orthonormal.image.w).foreach { x =>
      val v = orthonormal.image(x, staffY6) max
              orthonormal.image(x, staffY6 - 1) max
              orthonormal.image(x, staffY6 + 1)
      cutoff(x) = (v * 2) min 255
    }

    def updateCutoff(y:Int) {
      (0 until orthonormal.image.w).foreach { x =>
        val v = orthonormal.image(x, y)
        //if (cutoff(x) > 16 && v < cutoff(x) + 128) {
        //  if (v < cutoff(x))
        //    cutoff(x) = cutoff(x)*3/4 + v*1/4
        //}
        if (v >= cutoff(x) + 64) {
          //cutoff(x) = 0
          //demo(x, y) = ((v * 2) min 255, 0, 0)
          text(x, y) = (v - cutoff(x), 0, 0)
        }
        if (v < cutoff(x))
          cutoff(x) = cutoff(x)*7/8 + v*1/8
      }
    }

    (staffY6 until orthonormal.image.h).foreach { y =>
      updateCutoff(y)
    }

    val staffYNeg6 = orthonormal.yForStaffY(-6)
    (0 until orthonormal.image.w).foreach { x =>
      val v = orthonormal.image(x, staffYNeg6) max
        orthonormal.image(x, staffYNeg6 - 1) max
        orthonormal.image(x, staffYNeg6 + 1)
      cutoff(x) = (v * 2) min 255
    }
    val ceiling = new GrayImage(orthonormal.image.w, orthonormal.image.h)
    (staffYNeg6 to 0 by -1).foreach { y =>
      updateCutoff(y)
    }

    var y = 0
    var consecutiveNonTextRows = 0
    while (y < staffYNeg6 && consecutiveNonTextRows < 5) {
      var sumAllV = 0
      var sumNonTextV = 0
      (0 until text.w).foreach { x =>
        val (allV, nonTextV, _) = text(x, y)
        sumAllV += (allV - 64) max 0
        sumNonTextV += (nonTextV - 64) max 0
      }
      if (sumNonTextV > 50 && sumNonTextV * 100 / sumAllV > 20)
        consecutiveNonTextRows += 1
      else
        consecutiveNonTextRows = 0
      y += 1
    }
    val minY = (y - consecutiveNonTextRows) max 0

    y = text.h - 1
    consecutiveNonTextRows = 0
    while (y > staffY6 && consecutiveNonTextRows < 5) {
      var sumAllV = 0
      var sumNonTextV = 0
      (0 until text.w).foreach { x =>
        val (allV, nonTextV, _) = text(x, y)
        sumAllV += (allV - 64) max 0
        sumNonTextV += (nonTextV - 64) max 0
      }
      if (sumNonTextV > 50 && sumNonTextV * 100 / sumAllV > 20)
        consecutiveNonTextRows += 1
      else
        consecutiveNonTextRows = 0
      y -= 1
    }
    val maxY = (y + consecutiveNonTextRows) min (text.h - 1)
    
    //(0 until text.w).foreach { x =>
    //  text(x, minY) = (text(x, minY)._1, text(x, minY)._2, 255)
    //  text(x, maxY) = (text(x, maxY)._1, text(x, maxY)._2, 255)
    //}
    //text.saveTo(new File("demos/staffy4.%s.png".format(caseName)))
    (minY, maxY)
  }

  def saveWidths(boxes:List[BoundingBox], file:File) = {
    printToFile(file) { writer =>
      writer.println("MinX,MaxX,MinY,MaxY")
      boxes.foreach { box =>
        writer.println("%s,%s,%s,%s".format(
          box.minX, box.maxX, box.minY, box.maxY))
      }
    }
  }

  def powerSet[T](elements:List[T]) : List[Set[T]] = {
    elements match {
      case Nil => List(Set[T]())
      case first :: rest =>
        val recursive = powerSet(rest)
        recursive.map { _ + first } ++ recursive
    }
  }

  def prepareTemplate(templateName:String, cSpacing:Float) : GrayImage = {
    val templateName = "black_head"
    val templatePath = new File("templates/%s.png".format(templateName))
    val bigTemplate =
      ColorImage.readFromFile(templatePath).toGrayImage.inverse
    val templateW = 16
    val heightInStaffLines = 1.0f
    val templateH = Math.round(heightInStaffLines * cSpacing).intValue
    val template = scaleTemplate(bigTemplate, templateW, templateH)
    template
  }

  def findNotesInColumn(box:BoundingBox, orthonormal:Orthonormal,
      template:GrayImage, caseName:String) : List[TemplateMatch] = {
    val midX = (box.minX + box.maxX) / 2
    val possibleStaffYs = (-8 to 8).toList
    val possiblePoints = possibleStaffYs.map { staffY =>
      (midX, orthonormal.yForStaffY(staffY), staffY)
    }
    val results = findBlackHeads(orthonormal.image, template,
      possiblePoints, caseName)
    var foundNotes:List[TemplateMatch] = Nil
    (0 until results.size).foreach { i =>
      val (centerX, centerY, staffY) = possiblePoints(i)
      val result = results(i)
      if (result >= 0.2f)
        foundNotes = TemplateMatch(centerX, centerY, template.w, template.h,
          staffY, "black_head") :: foundNotes
    }
    foundNotes
  }

  def matchBoxesToAnnotations(boxes:List[BoundingBox], annotation:Annotation,
      transformXY:(Int,Int)=>(Int,Int)) = {
    var boxToAnnotatedStaffYs = Map[BoundingBox,Set[Int]]()
    var missedPoints = annotation.points
    boxes.foreach { box =>
      val points = annotation.points.filter { point =>
        val (x, y) = transformXY(point.x, point.y)
        val matches = x >= (box.minX - 5) && x <= (box.maxX + 5)
        //if (matches && !missedPoints.contains(point))
        //  throw new RuntimeException(
        //    "Too many bounding boxes for annotation %s".format(point))
        matches
      }
      missedPoints = missedPoints.filter { !points.contains(_) }
      val staffYs = points.map { _.staffY }.toSet
      boxToAnnotatedStaffYs = boxToAnnotatedStaffYs.updated(box, staffYs)
    }
    if (missedPoints.size > 0) {
      //throw new RuntimeException(
      //  "Couldn't find bounding box for annotations: %s".format(missedPoints))
    }
    boxToAnnotatedStaffYs
  }

  def processCase(caseName:String) : Performance = {
    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage
    val annotationPath = "input/%s.json".format(caseName)
    val annotationString = scala.io.Source.fromFile(annotationPath).mkString
    val annotation = loadAnnotationJson(annotationString)

    val (inputAdjusted, partiallyErased, augmentedBinaryNonStaff) =
      separateNotes(image, caseName)

    val metricsPath = new File("output/metrics/%s.json".format(caseName))
    val metrics = readOrGenerate(metricsPath, saveMetrics, restoreMetrics) {
      () => estimateMetrics(partiallyErased, caseName)
    }

    val yCorrection = determineYCorrection(
      partiallyErased, augmentedBinaryNonStaff, metrics, caseName)
    val (justNotes, justNotes2) =
      eraseStaffLines(image, augmentedBinaryNonStaff,
      metrics, yCorrection, caseName)

    val thickLines = findThickHorizontalLines(justNotes, metrics, caseName)
    val beams = findBeams(thickLines, image, caseName)
    demoBeams(beams, image, caseName)

    val justNotesNoBeams = eraseBeams(justNotes2, beams, metrics)
/*
    val segments = scanSegments(justNotesNoBeams)
    val shapes = groupTouchingSegments(segments)
    val shapeGroups = groupXOverlappingShapes(shapes)
    val mergedShapes = shapeGroups.map { shapes =>
      shapes.foldLeft(List[Segment]()) { _ ++ _ }
    }
    demoSegmentGroups(mergedShapes, image, caseName)
*/

    val inverseSlopeRange =
      findVLineInverseSlopeRange(justNotes2, image, caseName)
    val vlines =
      doVLineDetection(justNotes2, image, inverseSlopeRange, caseName)

    val orthonormal = orthonormalize(justNotesNoBeams, inverseSlopeRange,
      metrics, yCorrection, caseName)
    val boxes = doWidthDetection(orthonormal, vlines, caseName)
    saveWidths(boxes, new File("output/widths/%s.txt".format(caseName)))

    var predictedNotes:List[Set[TemplateMatch]] = Nil
    val boxToAnnotatedStaffYs =
      matchBoxesToAnnotations(boxes, annotation, orthonormal.transformXY)
    boxes.sortBy { _.minX }.foreach { box =>
      val width = box.maxX - box.minX + 1
      val annotatedStaffYs = boxToAnnotatedStaffYs(box)
      val prediction =
        if (width >= 18)
          Set[TemplateMatch]() // clef
        else if (width >= 5) {
          val template = prepareTemplate("black_head", orthonormal.cSpacing)
          val templates = Map("black_head" -> template)
          val foundNotes =
            findNotesInColumn(box, orthonormal, template, caseName)
          chooseBestOverlappingSets(
            box, foundNotes, templates, orthonormal.image)
        }
        else
          Set[TemplateMatch]() // measure line
      predictedNotes ++= List(prediction)
    }
    val filteredNotes = predictedNotes

    var casePerformance = Performance(List(), List(), List())
/*
    val c = metrics.cSpacing.intValue
    val templateSpecs =
      //TemplateSpec("treble_clef",   3,    8, findTrebleClef, 0.1) ::
      //TemplateSpec("sharp",       1.3,  2.6, findAccidental) ::
      //TemplateSpec("flat",        1.1, 2.35, findAccidental) ::
      //TemplateSpec("natural",       1,    3, findAccidental) ::
      TemplateSpec("black_head",   2.00, 1.25, findBlackHeads, 0.25) ::
      //TemplateSpec("white_head",  1.5, 1.25, findWhiteHeads, 1.0) ::
      Nil
    var casePerformance = Performance(List(), List(), List())
    var points:List[TemplateMatch] = Nil
    var templates = Map[String,GrayImage]()
    templateSpecs.foreach { templateSpec =>
      val templateName = templateSpec.name
      printf(" Searching for %s...\n", templateName)

      val templatePath = new File("templates/%s.png".format(templateName))
      val bigTemplate =
        ColorImage.readFromFile(templatePath).toGrayImage.inverse
      val templateW =
        (templateSpec.widthInStaffLines * metrics.cSpacing).intValue
      val templateH =
        (templateSpec.heightInStaffLines * metrics.cSpacing).intValue
      val template = scaleTemplate(bigTemplate, templateW, templateH)
      templates = templates.updated(templateName, template)
      val detected = templateSpec.finder(
        justNotes, template, templateName + "." + caseName)
      detected.saveTo(new File("demos/detected.%s.%s.png".format(
        templateName, caseName)))

      val thresholdInt =
        (templateSpec.threshold * templateW * templateH).intValue
      val detectedThreshold =
        ColorImage.giveRGBPerPixel(detected.w, detected.h) { (x, y) =>
          val v = detected(x, y)
          if (v < thresholdInt) (v * 255 / thresholdInt, 0, 0)
          else (v, v, v)
        }
      detectedThreshold.saveTo(new File("demos/detected2.%s.%s.png".format(
        templateName, caseName)))
      points ++= gleanPoints(detected, metrics, yCorrection, templateW,
        templateH, thresholdInt, templateName)
    }

    val overlappingPointGroups = groupOverlappingPoints(points)
    demoAlternatives(overlappingPointGroups, inputAdjusted, caseName)
    demoPointGroups(overlappingPointGroups, inputAdjusted, caseName)
    val culledPointGroups = chooseBestOverlappingSets(
      overlappingPointGroups, templates, justNotes.inverse)

    val groupedPoints = groupTemplateMatches(culledPointGroups)
    val filteredNotes = groupedPoints

    val demo = inputAdjusted.toColorImage
    filteredNotes.foreach { noteGroup =>
      noteGroup.foreach { point =>
        drawTemplateMatch(point, demo, templates(point.templateName))
      }
    }
    demo.saveTo(new File("demos/notes.%s.png".format(caseName)))
*/
    demoNotes(filteredNotes, image.toColorImage, caseName)

    val realNotes = filteredNotes.map { _.filter { note =>
      note.templateName == "black_head" || note.templateName == "white_head" } }
    val performance = calcPerformance(realNotes, annotation.notes)
    println("Case %2s: precision: %.3f, recall: %.3f".format(
      caseName, performance.precision, performance.recall))
    //printf("  correct: %s\n", performance.correctNotes)
    if (performance.spuriousNotes.size > 0)
      printf("  spurious: %s\n", performance.spuriousNotes)
    if (performance.missingNotes.size > 0)
      printf("  missing: %s\n", performance.missingNotes)

    casePerformance = Performance(
      casePerformance.correctNotes ++ performance.correctNotes,
      casePerformance.spuriousNotes ++ performance.spuriousNotes,
      casePerformance.missingNotes ++ performance.missingNotes)
    casePerformance
  }

  def edgeDetection(input:GrayImage, matrix:Array[Int]) = {
    val output = new GrayImage(input.w, input.h)
    val denom = matrix(9)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val sum =
          (matrix(0) * input(x - 1, y - 1) +
           matrix(1) * input(x + 0, y - 1) +
           matrix(2) * input(x + 1, y - 1) +
           matrix(3) * input(x - 1, y + 0) +
           matrix(4) * input(x + 0, y + 0) +
           matrix(5) * input(x + 1, y + 0) +
           matrix(6) * input(x - 1, y + 1) +
           matrix(7) * input(x + 0, y + 1) +
           matrix(8) * input(x + 1, y + 1) +
           0) / denom
        output(x, y) = sum
      }
    }
    output
  }

  def distance(input:GrayImage, threshold:Int) = {
    // Distance from the top left
    val fromTL = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        if (input(x, y) < threshold)
          fromTL(x, y) = 0
        else {
          var candidates:List[Int] = List(input.w max input.h) // infinity

          if (y > 0 && x > 0)
            candidates = fromTL(x - 1, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y > 0)
            candidates = fromTL(x, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y > 0 && x < input.w - 1)
            candidates = fromTL(x + 1, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (x > 0)
            candidates = fromTL(x - 1, y) + 1 :: candidates
          else
            candidates = 1 :: candidates

          fromTL(x, y) = candidates.min
        }
      }
    }

    // Distance from the bottom right
    val fromBR = new GrayImage(input.w, input.h)
    (input.h - 1 to 0 by -1).foreach { y =>
      (input.w - 1 to 0 by -1).foreach { x =>
        if (input(x, y) < threshold)
          fromBR(x, y) = 0
        else {
          var candidates:List[Int] = List(input.w max input.h) // infinity

          if (y < input.h - 1 && x < input.w - 1)
            candidates = fromBR(x + 1, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y < input.h - 1)
            candidates = fromBR(x, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y < input.h - 1 && x > 0)
            candidates = fromBR(x - 1, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (x < input.w - 1)
            candidates = fromBR(x + 1, y) + 1 :: candidates
          else
            candidates = 1 :: candidates

          fromBR(x, y) = candidates.min
        }
      }
    }

    val fromEither = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        fromEither(x, y) = fromTL(x, y) min fromBR(x, y)
      }
    }
    fromEither
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)

      var globalPerformance = Performance(List(), List(), List())
      val caseNames = expandCaseNames(args)
      caseNames.foreach { caseName =>
        println("Case %s:".format(caseName))
        val performance = processCase(caseName)
        globalPerformance = Performance(
          globalPerformance.correctNotes ++ performance.correctNotes,
          globalPerformance.spuriousNotes ++ performance.spuriousNotes,
          globalPerformance.missingNotes ++ performance.missingNotes)
      }
      println("Total:   precision: %.3f -- recall: %.3f".format(
        globalPerformance.precision, globalPerformance.recall))

    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      println(Colors.ansiEscapeNormal)
    }
    System.exit(0)
  }
}
