import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D
import com.twitter.json.Json
import scala.collection.mutable.ArraySeq
import scala.io.Source

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val annotation = (255, 0, 0)
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

case class LabeledBoundingBox (
  val label:BoundingBoxLabel,
  val box:BoundingBox
) {}

sealed abstract class BoundingBoxLabel
case object Note extends BoundingBoxLabel
case object NonNote extends BoundingBoxLabel

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
  val correctNotes:List[(Int,Int)],
  val spuriousNotes:List[(Int,Int)],
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

case class NoteColumn (
  val skew:Int,
  val x0:Int,
  val x1:Int,
  val strength:Int
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
  val label:String
) {}

object Ocr4Music {
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def findArgmax[A,B <% Ordered[B]](inputs:Seq[A], block:A=>B) : (A, Int) = {
    var argmaxIndex = 0
    var argmax = inputs(0)
    var max = block(inputs(0))
    (0 until inputs.length).foreach { i =>
      val input = inputs(i)
      val output = block(input)
      if (output > max) {
        max = output
        argmax = input
        argmaxIndex = i
      }
    }
    (argmax, argmaxIndex)
  }

  def averageOfSkewedImage(image:GrayImage, skew:Int, x0:Int, x1:Int)
      : Array[Int] = {
    val sum:Array[Int] = new Array[Int](image.h)
    val inverseImage = image.inverse // so out of bounds is white not black
    (x0 until x1).foreach { x =>
      val skewAmount = x * skew / image.w
      (0 until image.h).foreach { y =>
        sum(y) += inverseImage(x, y + skewAmount)
      }
    }
    val average = sum.map { v => 255 - (v / image.w) }
    average
  }

  def maxVerticalSkewGivenWidth(width:Int) : Int = { width / 2 }

  def findBestSkew(image:GrayImage, x0:Int, x1:Int) : Int = {
    val maxSkew = maxVerticalSkewGivenWidth(image.w)
    val (bestSkew, _) = findArgmax[Int,Int]((-maxSkew to maxSkew), { skew =>
      val average = averageOfSkewedImage(image, skew, x0, x1)
      val score = average.max - average.min
      score
    })
    bestSkew
  }

  def constructImageComparingSkews(imageIn:GrayImage) : GrayImage = {
    val maxSkew = maxVerticalSkewGivenWidth(imageIn.w)
    var skews = (-maxSkew to maxSkew).map { skew =>
      averageOfSkewedImage(imageIn, skew, 0, imageIn.w)
    }
    var w = maxSkew * 2 + 1
    var imageOfSkews = new GrayImage(w, imageIn.h)
    for (y <- 0 until imageIn.h) {
      for (x <- 0 until w) {
        imageOfSkews(x, y) = skews(x)(y)
      }
    }
    imageOfSkews
  }

  def annotateSkew(skew:Int, image:GrayImage) {
    var annotated = image.toColorImage
    for (x <- 0 until image.w) {
      val skewAmount = x * skew / image.w
      annotated(x, (image.h / 2) + skewAmount) = Colors.annotation
    }
    annotated.saveTo(new File("skew.png"))
  }

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
    hough2.saveTo(new File("hough-%s.png".format(caseName)))

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

  def annotateCenterY(input:GrayImage, metrics:Metrics,
      yCorrection:Array[Float]) : ColorImage = {
    val annotated = input.toColorImage
    val color = (255,255,255) // white
    val halfW = annotated.w / 2
    (-halfW until halfW).foreach { x =>
      (-2 to 2).foreach { staffY =>
        val a = metrics.a
        val b = metrics.b + (staffY * metrics.bSpacing)
        val c = metrics.c + (staffY * metrics.cSpacing)
        val y = Math.round((a * x * x + b * x + c) + yCorrection(x + halfW) +
          (input.h / 2)).intValue
        if (y >= 0 && y < annotated.h)
          annotated(x + halfW, y) = (255, 255, 255)
      }
    }
    annotated
  }

  def scanSegments(binaryImage:GrayImage) : List[Segment] = {
    var segments:List[Segment] = Nil
    (0 until binaryImage.h).foreach { y =>
      var previousPixel = 255
      var openSegment = 0
      (0 until binaryImage.w).foreach { x =>
        val currentPixel = binaryImage(x, y)
        if (currentPixel == 0) { // 0 is "on", by the way
          if (previousPixel != 0)
            openSegment = x
        }
        else {
          if (previousPixel == 0)
            segments = Segment(y, openSegment, x) :: segments
        }
        previousPixel = currentPixel
      }
    }
    segments
  }

  def annotateSegments(baseImage:GrayImage, segments:List[Segment]) {
    val annotated = baseImage.toColorImage
    segments.foreach { segment =>
      annotated(segment.x0, segment.y) = Colors.annotation
      annotated(segment.x1, segment.y) = Colors.annotation
    }
    annotated.saveTo(new File("segments.png"))
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
        val touchingGroupIfAny = activeGroups.find { group =>
          group.previousLayer.find { previousSegment =>
            segment.x0 < previousSegment.x1 && segment.x1 > previousSegment.x0
          }.isDefined
        }
        val group = touchingGroupIfAny.getOrElse {
          val newGroup = SegmentGroup(Nil, Nil, Nil)
          activeGroups = newGroup :: activeGroups
          newGroup
        }
        group.currentLayer = segment :: group.currentLayer
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

  def annotateBounds(baseImage:ColorImage, bounds:List[LabeledBoundingBox]) = {
    val annotated = baseImage.copy
    bounds.foreach { bound =>
      val color = if (bound.label == Note) (0,255,0) else (255,0,0)
      val density = if (bound.label == Note) 1 else 3
      val box = bound.box
      (box.minX to box.maxX by density).foreach { x =>
        annotated(x, box.minY) = color
        annotated(x, box.maxY) = color
      }
      (box.minY to box.maxY by density).foreach { y =>
        annotated(box.minX, y) = color
        annotated(box.maxX, y) = color
      }
    }
    annotated
  }

  def annotateNotes(
      noteGroups:List[List[Int]], input:ColorImage, caseName:String) {
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
    noteGroup.foreach { staffY =>
    
      val centerY = (staffHeight / 2) +
        (staffY * staffSeparation / 2).intValue
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

    image.saveTo(new File("notes-%s.png".format(caseName)))
  }

  def combineNoteBounds(bounds:List[LabeledBoundingBox]) = {
    val boundsNotes = bounds.filter { bound =>
      bound.label == Note
    }

    def findOverlappingBound(
        bound1:LabeledBoundingBox, boundsList:List[LabeledBoundingBox])
        : Option[LabeledBoundingBox] = {
      boundsList.foreach { bound2 =>
        if (bound1.box.minX <= bound2.box.maxX &&
            bound1.box.maxX >= bound2.box.minX &&
            bound1.box.minY <= bound2.box.maxY &&
            bound1.box.maxY >= bound2.box.minY)
          return Some(bound2)
      }
      return None
    }
    var boundsCombined:List[LabeledBoundingBox] = List[LabeledBoundingBox]()
    boundsNotes.foreach { bound1 =>
      findOverlappingBound(bound1, boundsCombined) match {
        case Some(bound2) =>
          val newBound = LabeledBoundingBox(Note, BoundingBox(
            bound1.box.minX min bound2.box.minX,
            bound1.box.maxX max bound2.box.maxX,
            bound1.box.minY min bound2.box.minY,
            bound1.box.maxY max bound2.box.maxY))
          boundsCombined =
            newBound :: (boundsCombined - bound2)
        case None =>
          boundsCombined = bound1 :: boundsCombined
      }
    }
    boundsCombined
  }

  def recognizeNotesFromTemplateMatches(
      matches:List[TemplateMatch], metrics:Metrics,
      yCorrection:Array[Float]) = {
    val matchesSorted = matches.sort { (match1, match2) =>
      match1.x < match2.x
    }

    var noteGroups:List[List[Int]] = Nil
    var currentNoteGroup:List[Int] = Nil
    var lastNoteX = -1
    matchesSorted.foreach { _match =>
      val xCentered = _match.x - metrics.w / 2
      val yCentered = _match.y - metrics.h / 2
      val displacementY =
        metrics.a * xCentered * xCentered + metrics.b * xCentered + metrics.c
      val deskewedY = yCentered - displacementY - yCorrection(_match.x)
      val staffSeparation = (xCentered * metrics.bSpacing) + metrics.cSpacing
      val staffY = deskewedY / (staffSeparation / 2)
        
      if (Math.abs(_match.x - lastNoteX) >= 14 && currentNoteGroup.size > 0) {
        noteGroups = currentNoteGroup :: noteGroups
        currentNoteGroup = Nil
      }

      currentNoteGroup = Math.round(staffY) :: currentNoteGroup

      lastNoteX = _match.x
    }
    if (currentNoteGroup.size > 0)
      noteGroups = currentNoteGroup :: noteGroups

    noteGroups.reverse
  }

  def labelBounds(
      bounds:List[BoundingBox], justNotes:GrayImage, metrics:Metrics) = {
    bounds.map { bound =>
      val w = bound.maxX - bound.minX + 1
      val h = bound.maxY - bound.minY + 1
      if (w / h > 6)
        LabeledBoundingBox(NonNote, bound) // beam connecting eighth notes
      else
        LabeledBoundingBox(Note, bound)
    }
  }

  def demoExtremes(input:GrayImage, metrics:Metrics) {
    val expectedNoteW = 15
    val expectedNoteH = 10
    val blackest = 50
    val whitest = 100
    val newImage = new ColorImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        val v = (input(x, y) - blackest) * 255 / (whitest - blackest)
        newImage(x, y) = 
          if (v > 255) (255, 0, 0)
          else if (v < 0) (0, 0, 255)
          else (v, v, v)
      }
    }
    newImage.saveTo(new File("extremes.png"))

    val matchImage = new GrayImage(input.w, input.h)
    (-5 to 5).foreach { staffYDoubled =>
      val x0 = expectedNoteW/2
      val x1 = input.w - expectedNoteW/2
      (x0 until x1).foreach { x =>
        val xCentered = x - (input.w / 2)
        val a = metrics.a
        val b = metrics.b + (staffYDoubled * metrics.bSpacing / 2)
        val c = metrics.c + (staffYDoubled * metrics.cSpacing / 2)
        val y = Math.round(
            (a * xCentered * xCentered + b * xCentered + c) + (input.h / 2)
          ).intValue
        if (y - expectedNoteH/2 >= 0 &&
            y + expectedNoteH/2 < input.h) {
          var sumDarkness = 0
          var xWeight = 0
          var yWeight = 0
          (-expectedNoteH/2 until expectedNoteH/2).foreach { deltaY =>
            (-expectedNoteW/2 until expectedNoteW/2).foreach { deltaX =>
              val oldV = input(x + deltaX, y + deltaY)
              val scaledV = (oldV - blackest) * 255 / (whitest - blackest)
              val truncated =
                (if (scaledV > 255) 0
                else if (scaledV < 0) 255
                else 255 - scaledV)
              sumDarkness += truncated
              xWeight += truncated * deltaX
              yWeight += truncated * deltaY
            }
          }
          val xAdjustment = xWeight /
            (expectedNoteW * expectedNoteH * expectedNoteW)
          val v = sumDarkness / (expectedNoteW * expectedNoteH)
          val newY = staffYDoubled * metrics.cSpacing / 2 + (input.h / 2)
          val yAdjustment = yWeight /
            (expectedNoteW * expectedNoteH * expectedNoteH)
          matchImage(x + xAdjustment / 3, newY.intValue + yAdjustment / 5) =
            matchImage(x + xAdjustment / 3, newY.intValue + yAdjustment / 5) + v
        }
      }
    }

    matchImage.saveTo(new File("match.png"))
  }

  def findNoteColumns(justNotes:GrayImage, caseNum:Int) = {
    val justNotesRotated = new GrayImage(justNotes.h, justNotes.w)
    (0 until justNotesRotated.w).foreach { x =>
      (0 until justNotesRotated.h).foreach { y =>
        justNotesRotated(x, y) = justNotes(y, x)
      }
    }
    val skewDemo = constructImageComparingSkews(justNotesRotated)
    skewDemo.saveTo(new File("skew_demo%d.png".format(caseNum)))

    val skewDemoSlope = new ColorImage(skewDemo.w, skewDemo.h)
    (1 until skewDemo.h).foreach { y =>
      (0 until skewDemo.w).foreach { x =>
        val above = skewDemo(x, y - 1)
        val below = skewDemo(x, y)
        if (above > below)
          skewDemoSlope(x, y) = ((above - below) * 2, 0, 0)
        else
          skewDemoSlope(x, y) = (0, 0, (below - above) * 3)
      }
    }
    skewDemoSlope.saveTo(new File("skew_demo_slope%d.png".format(caseNum)))

    var bestX = 0
    var maxCombination = 0
    (0 until skewDemo.w).foreach { x =>
      var maxIncrease = 0
      var maxDecrease = 0
      (1 until skewDemo.h).foreach { y =>
        val above = skewDemo(x, y - 1)
        val below = skewDemo(x, y)
        if (above > below) {
          val increase = above - below
          if (increase > maxIncrease)
            maxIncrease = increase
        }
        else {
          val decrease = below - above
          if (decrease > maxDecrease)
            maxDecrease = decrease
        }
      }
      val combination = maxIncrease + maxDecrease
      if (combination > maxCombination) {
        maxCombination = combination
        bestX = x
      }
    }
    val bestSkew = bestX - skewDemo.w/2

    var bestShift = 0
    var maxContrast = 0
    (4 to 30).foreach { shift =>
      (1 until skewDemoSlope.h - shift).foreach { y0 =>
        val y1 = y0 + shift
        val y0Increase = skewDemoSlope(bestX, y0)._1
        val y1Decrease = skewDemoSlope(bestX, y1)._3
        val contrast = y0Increase + y1Decrease
        if (contrast > maxContrast) {
          maxContrast = contrast
          bestShift = shift
        }
      }
    }

    var noteColumns:List[NoteColumn] = Nil
    (1 until skewDemoSlope.h - bestShift).foreach { y0 =>
      val y1 = y0 + bestShift
      val y0Increase = skewDemoSlope(bestX, y0)._1
      val y1Decrease = skewDemoSlope(bestX, y1)._3
      val contrast = y0Increase + y1Decrease
      if (y0Increase > 0 && y1Decrease > 0)
        noteColumns =
          NoteColumn(bestSkew, y0 + bestSkew / 2, y1 + bestSkew / 2,
            y0Increase min y1Decrease) :: noteColumns
    }
    noteColumns
  }

  def demoNoteColumns(
      noteColumns:List[NoteColumn], input:GrayImage, caseNum:Int) {
    val demoImage = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        var numColumnsInside = 0
        noteColumns.foreach { noteColumn =>
          val NoteColumn(skew, x0, x1, strength) = noteColumn
          val xShifted = x + -skew * (y - input.h/2) / input.h/2
          if (xShifted >= x0 && xShifted <= x1)
            numColumnsInside += strength
        }
        demoImage(x, y) = input(x, y) + numColumnsInside / 2
      }
    }
    demoImage.saveTo(new File("note_columns%d.png".format(caseNum)))
  }

  def findDarkSpots(input:GrayImage, metrics:Metrics, caseNum:Int) = {
    val threshold = 200
    val spots = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        // Differentiate axx + bx + c to get 2ax + b as slope
        val xCentered = x - metrics.w/2
        val slope = (2.0f * metrics.a * xCentered) + metrics.b
        val staffSeparation = (xCentered * metrics.bSpacing) + metrics.cSpacing

        var sumFilled = 0
        var opportunities = 0
        (-8 to 8).foreach { noteX =>
          (-8 to 8).foreach { noteY =>
            val newX = x + noteX
            val newY = y + noteY + Math.round(noteX * slope).intValue
            if ((noteX * noteX) + 2 * (noteY * noteY) <
                staffSeparation * staffSeparation * 0.5f) {
              if (newX >= 0 && newX < spots.w &&
                  newY >= 0 && newY < spots.h) {
                val v = input(newX, newY)
                sumFilled += (if (v > threshold) 0 else 1)
              }
              opportunities += 1
            }
          }
        }
        val isNote = (sumFilled.floatValue / opportunities > 0.9f)
        spots(x, y) = (if (isNote) 255 else 0)
      }
    }

    val demo = new ColorImage(input.w, input.h)
    (0 until spots.w).foreach { x =>
      (0 until spots.h).foreach { y =>
        val v = input(x, y)
        demo(x, y) = (v, v, v)
      }
    }
    (0 until spots.w).foreach { x =>
      (0 until spots.h).foreach { y =>
        val xCentered = x - metrics.w/2
        val slope = (2.0f * metrics.a * xCentered) + metrics.b
        val staffSeparation = (xCentered * metrics.bSpacing) + metrics.cSpacing
        if (spots(x, y) == 255) {
          (-8 to 8).foreach { noteX =>
            (-8 to 8).foreach { noteY =>
              val newX = x + noteX
              val newY = y + noteY + Math.round(noteX * slope).intValue
              if (newX >= 0 && newX < spots.w &&
                  newY >= 0 && newY < spots.h &&
                  (noteX * noteX) + 2 * (noteY * noteY) <
                  staffSeparation * staffSeparation * 0.5f) {
                demo(newX, newY) = (255, 0, 0)
              }
            }
          }
        }
      }
    }
    (0 until spots.w).foreach { x =>
      (0 until spots.h).foreach { y =>
        if (spots(x, y) == 255) {
          demo(x, y) = (255, 255, 0)
        }
      }
    }
    demo.saveTo(new File("dark_spots%d.png".format(caseNum)))

    val darkSpots = new GrayImage(input.w, input.h)
    (0 until spots.w).foreach { x =>
      (0 until spots.h).foreach { y =>
        darkSpots(x, y) = (if (spots(x, y) == 255) 0 else 1)
      }
    }
    darkSpots
  }

/*
  def recognizeNotes(original:GrayImage, box:Annotation,
      points:List[LabeledPoint], caseNum:Int) = {
    val excerpt = original.crop(box.left, box.top, box.width, box.height)
    val (justNotes, whiteBackground, partiallyErased, augmentedBinaryNonStaff) =
      separateNotes(excerpt)
    partiallyErased.saveTo(new File("partially_erased.png"))
    justNotes.saveTo(new File("just_notes%d.png".format(caseNum)))
    whiteBackground.saveTo(new File("white_background%d.png".format(caseNum)))
    val metrics = estimateMetrics(partiallyErased, caseNum)
    val yCorrection = determineYCorrection(
      partiallyErased, augmentedBinaryNonStaff, metrics, caseNum)
    demoStaffLines(excerpt, metrics, yCorrection, caseNum)

    val darkSpots = findDarkSpots(whiteBackground, metrics, caseNum)

    val segments = scanSegments(darkSpots)
    val segmentGroups = groupTouchingSegments(segments)
    val bounds = boundSegmentGroups(segmentGroups)
    val labeledBounds = labelBounds(bounds, justNotes, metrics)
    val combinedBounds = combineNoteBounds(labeledBounds)
    val estimatedNotes = recognizeNotesFromBounds(
      combinedBounds, metrics, yCorrection)
    annotateNotes(estimatedNotes,
      annotateBounds(annotateCenterY(excerpt, metrics, yCorrection),
        labeledBounds), caseNum)
    estimatedNotes
  }
*/

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
      estimated:List[List[Int]], annotated:List[List[Int]]) = {
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
      val estimatedNotes:List[Int],
      val annotatedNotes:List[Int]
    ) {}
    var pairedNoteGroups:List[PairedNoteGroup] = Nil
    var x = w
    var y = h
    while (x > 0 || y > 0) {
      backPointer(y)(x) match {
        case (-1, 0) =>
          pairedNoteGroups = PairedNoteGroup(x - 1, estimated(x - 1), List()) ::
            pairedNoteGroups
          x -= 1
        case (0, -1) =>
          pairedNoteGroups = PairedNoteGroup(x - 1, List(), annotated(y - 1)) ::
            pairedNoteGroups
          y -= 1
        case (-1, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimated(x - 1), annotated(y - 1)) ::
            pairedNoteGroups
          x -= 1
          y -= 1
      }
    }
    pairedNoteGroups = pairedNoteGroups.reverse

    var correctNotes:List[(Int,Int)] = Nil
    var spuriousNotes:List[(Int,Int)] = Nil
    var missingNotes:List[(Int,Int)] = Nil
    pairedNoteGroups.foreach { pair =>
      val PairedNoteGroup(staffX, estimated, annotated) = pair
      estimated.foreach { i =>
        if (!(annotated.contains(i)))
          spuriousNotes = (staffX, i) :: spuriousNotes
        else
          correctNotes = (staffX, i) :: correctNotes
      }
      annotated.foreach { i =>
        if (!(estimated.contains(i)))
          missingNotes = (staffX, i) :: missingNotes
      }
    }

    Performance(correctNotes, spuriousNotes, missingNotes)
  }

  def demoStaffLines(excerpt:GrayImage, metrics:Metrics,
      yCorrection:Array[Float], caseName:String) {
    val demo = excerpt.resize(excerpt.w * 4, excerpt.h * 4, 0)
    (-2 to 2).foreach { staffY =>
      (0 until excerpt.w).foreach { xUncentered =>
        val x = xUncentered - (excerpt.w / 2)
        val a = metrics.a
        val b = metrics.b + (staffY * metrics.bSpacing)
        val c = metrics.c + (staffY * metrics.cSpacing)
        val y = (a * x * x + b * x + c) + yCorrection(xUncentered) +
          (excerpt.h / 2)
        if (y >= 0 && Math.round(y * 4).intValue < demo.h)
          demo(xUncentered * 4, Math.round(y * 4).intValue) = 255

        /*val b = metrics.staffYToB(staffY + 2)
        val c = metrics.staffYToC(staffY + 2)
        val y = (a * x * x + b * x + c) + (excerpt.h / 2)
        if (y >= 0 && Math.round(y * 4).intValue < demo.h)
          demo(xUncentered * 4, Math.round(y * 4).intValue) = 255*/
      }
    }
    demo.saveTo(new File("staff_lines-%s.png".format(caseName)))
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
      (-2 to 2).foreach { staffY =>
        val x = xUncentered - (partiallyErased.w / 2)
        val a = metrics.a
        val b = metrics.b + (staffY * metrics.bSpacing)
        val c = metrics.c + (staffY * metrics.cSpacing)
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
        //justStaff(xUncentered, 10) = 150
        //justStaff(xUncentered, (averageY * 5).intValue + 10) = 0
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

  def findBestMatchAroundPoint(templateScaledMatrix:Array[Array[GrayImage]],
      inputAdjusted:GrayImage,
      expectedX:Int, expectedY:Int, metrics:Metrics, label:String) = {

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

    findBestMatch(templateScaledMatrix, inputAdjusted,
      (minX, maxX), (minY, maxY), metrics, label)
  }

  def findBestMatch(templateScaledMatrix:Array[Array[GrayImage]],
      inputAdjusted:GrayImage,
      minXmaxX:(Int,Int), minYmaxY:(Int,Int), 
      metrics:Metrics, label:String) = {
    val (minX, maxX) = minXmaxX
    val (minY, maxY) = minYmaxY

    // Differentiate axx + bx + c to get 2ax + b as slope
    val expectedX = (minX + maxX) / 2
    val xCentered = expectedX - metrics.w / 2
    val slope = (2.0f * metrics.a * xCentered) + metrics.b
    val staffSeparation =
      ((xCentered * metrics.bSpacing) + metrics.cSpacing).intValue

    val (minW, maxW) = (staffSeparation, staffSeparation * 3)
    val (minH, maxH) = label match {
      case "L" | "S" | "Sa" | "Sb" | "2" =>
        (staffSeparation, staffSeparation * 3/2)
      case "#" =>
        (staffSeparation * 3, staffSeparation * 5)
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
      //val templateScaled = scaleTemplate(template, templateW, templateH)
      val templateScaled = templateScaledMatrix(templateH)(templateW)

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
              //val templateV = template(
              //  templateScaledX * template.w / templateW,
              //  templateScaledY * template.h / templateH)
              val templateV = templateScaled(templateScaledX, templateScaledY)
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
//if (meanBlackMatch > 50 && meanWhiteMatch > 50 && meanBlackMatchY < 3 && meanBlackMatchX < 5)
  ////demo2(inputCenterX, inputCenterY) = (meanBlackMatch * 2, 0, meanWhiteMatch * 2)

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
      slope, label)
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

  def printMatch(writer:java.io.PrintWriter, point:TemplateMatch) {
    writer.print(point.x)
    writer.print("," + point.y)
    writer.print("," + point.w)
    writer.print("," + point.h)
    writer.print("," + point.blackMatch)
    writer.print("," + point.whiteMatch)
    writer.print("," + point.blackMatchX)
    writer.print("," + point.blackMatchY)
    writer.print("," + (point.blackMatch + point.whiteMatch))
    writer.print("," + point.slope)
    writer.print("," + point.label)
    writer.println()
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
      inputAdjusted.saveTo(new File("input_adjusted.png"))

      val templateBlackHeadScaledMatrix = (0 to 40).map { templateH =>
        (0 to 40).map { templateW =>
          scaleTemplate(templateBlackHead, templateW, templateH)
        }.toArray
      }.toArray
      val templateWhiteHeadScaledMatrix = (0 to 40).map { templateH =>
        (0 to 40).map { templateW =>
          scaleTemplate(templateWhiteHead, templateW, templateH)
        }.toArray
      }.toArray

      var points:List[TemplateMatch] = Nil
      List("L", "2").foreach { label =>
        //printf("Processing %s...\n", label)
        val templateScaledMatrix = label match {
          case "L" => templateBlackHeadScaledMatrix
          case "2" => templateWhiteHeadScaledMatrix
        }
        (0 until augmentedBinaryNonStaff.h).foreach { y =>
          (0 until augmentedBinaryNonStaff.w).foreach { x =>
            // remove the bands at the top and the bottom, which are probably
            // artifacts from the vertical blurring
            if (y > 4 && y < augmentedBinaryNonStaff.h - 5 &&
                augmentedBinaryNonStaff(x, y) == 0) {
              val newPoint = findBestMatch(templateScaledMatrix, inputAdjusted,
                  (x, x), (y, y), metrics, label)
              points = newPoint :: points
            }
          }
        }
      }

      val pointsFiltered = points.filter { point1 =>
        var hasBetterNeighbor = false
        points.foreach { point2 =>
          if (Math.abs(point2.x - point1.x) <= 3 &&
              Math.abs(point2.y - point1.y) <= 3 &&
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
          case "L" => point1.blackMatch > 102
          case "2" => point1.blackMatch > 65 && point1.whiteMatch > 90
        }

        !hasBetterNeighbor && strongEnough
      }

      val demo = input.toColorImage
      pointsFiltered.foreach { point =>
        val template = point.label match {
          case "L" => templateBlackHead
          case "2" => templateWhiteHead
        }
        drawTemplateMatch(point, demo, template)
      }
      demo.saveTo(new File("find_best_match.png"))

      val estimatedNotes = recognizeNotesFromTemplateMatches(
        pointsFiltered, metrics, yCorrection)
      val performance = calcPerformance(estimatedNotes, annotation.notes)
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
