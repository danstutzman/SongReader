import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D
import com.twitter.json.Json

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val annotation = (255, 0, 0)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
}

case class Metrics (
  val width:Int,
  val skew:Int,
  val waveLength:Float,
  val wavePhase:Float,
  val centerY:Float
) {}

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

case class Annotation (
  val left:Int,
  val top:Int,
  val width:Int,
  val height:Int,
  val notes:Set[Note]
) {}

case class Note (
  val staffX:Int, // 0 = far left, n = far right
  val staffY:Int // 0 = the middle, -4 = top, 4 = bottom
) {}

class Performance (
  val numCorrect:Int,
  val numIncorrect:Int,
  val numMissing:Int
) {
  def this() = this(0, 0, 0)
  def precision() = { numCorrect.floatValue / (numCorrect + numIncorrect) }
  def recall() = { numCorrect.floatValue / (numCorrect + numMissing) }
}

object Ocr4Music {
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

  def averageOfSkewedImage(image:GrayImage, skew:Int) : Array[Int] = {
    val sum:Array[Int] = new Array[Int](image.h)
    val inverseImage = image.inverse // so out of bounds is white not black
    (0 until image.w).foreach { x =>
      val skewAmount = x * skew / image.w
      (0 until image.h).foreach { y =>
        sum(y) += inverseImage(x, y + skewAmount)
      }
    }
    val average = sum.map { v => 255 - (v / image.w) }
    average
  }

  def maxVerticalSkewGivenWidth(width:Int) : Int = { width / 4 }

  def findBestSkew(image:GrayImage) : Int = {
    val maxSkew = maxVerticalSkewGivenWidth(image.w)
    val (bestSkew, _) = findArgmax[Int,Int]((-maxSkew to maxSkew), { skew =>
      val average = averageOfSkewedImage(image, skew)
      val score = average.max - average.min
      score
    })
    bestSkew
  }

  def constructImageComparingSkews(imageIn:GrayImage) : GrayImage = {
    val maxSkew = maxVerticalSkewGivenWidth(imageIn.w)
    var skews = (-maxSkew to maxSkew).map { skew =>
      averageOfSkewedImage(imageIn, skew)
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

  def hammingWindow(n:Int) : Seq[Double] = {
    (0 until n).map { i =>
      0.54 - 0.46 * Math.cos(2 * Math.PI * i / (n - 1))
    }
  }

  // returns image with just notes, and without notes
  def separateNotes(input:GrayImage) : (GrayImage, GrayImage) = {
    val whiteBackground = input.brighten(130)
    val binaryNonStaff = whiteBackground.blurVertically1.binarize(200)
    val augmentedBinaryNonStaff = binaryNonStaff.blurVertically4.binarize(254)
    val partiallyErased = whiteBackground.addWithCeiling(
      augmentedBinaryNonStaff.inverse)
    (binaryNonStaff, partiallyErased)
  }

  def annotateFFTResult(input:GrayImage, metrics:Metrics) {
    val annotated = input.toColorImage
    var y = metrics.waveLength * -metrics.wavePhase
    val color = (255,0,0) // red
    while (y < annotated.h) {
      for (x <- 0 until 20) {
        val skewedY = (y + (x * metrics.skew / annotated.w)).intValue
        if (skewedY >= 0 && skewedY < annotated.h)
          annotated(x, skewedY) = color
      }
      y += metrics.waveLength
    }
    annotated.saveTo(new File("annotated2.png"))
  }

  def estimateMetrics(partiallyErased:GrayImage) : Metrics = {
    val bestSkew = findBestSkew(partiallyErased)
    val bestSkewsAverage = averageOfSkewedImage(partiallyErased, bestSkew)

    val window = hammingWindow(bestSkewsAverage.length)
    val fftNumBuckets = 512
    val fftInput = new Array[Float](fftNumBuckets)
    (0 until bestSkewsAverage.length).foreach { i =>
      fftInput(i) = (255 - bestSkewsAverage(i)) * window(i).floatValue
    }

    val fftOutput = fftInput.clone
    val fft = new FloatFFT_1D(fftNumBuckets)
    fft.realForward(fftOutput) // mutates array in place

    val fftOutputPolar  = new Array[(Float,Float)](fftNumBuckets / 2)
    (0 until fftNumBuckets by 2).foreach { i =>
      val (re, im) = (fftOutput(i), fftOutput(i + 1))
      val magnitude = Math.sqrt(re * re + im * im)
      var phase = Math.atan2(im, re)
      fftOutputPolar(i / 2) =
        if (i / 2 > 20) (magnitude.floatValue, phase.floatValue)
        else (0.0f, 0.0f)
    }

    val (strongestBucketContents, strongestBucketNum) =
      findArgmax[(Float,Float),Float](fftOutputPolar, { polar => polar._1 })
    val waveLength = fftNumBuckets.floatValue / strongestBucketNum
    val wavePhase = (strongestBucketContents._2 / Math.PI / 2).floatValue

    var darkYs:List[Int] = Nil
    var darkY = waveLength * -wavePhase
    while (darkY < bestSkewsAverage.length) {
      if (darkY.intValue > 0)
        darkYs = darkY.intValue :: darkYs
      darkY += waveLength
    }

    val darkYIndexes = 2 until darkYs.length - 2
    val (bestCenterIndex, _) =
      findArgmax[Int,Int](darkYIndexes, { centerIndex =>
        (-2 to 2).foldLeft(0) { (sum, whichStaffLine) =>
          sum + -bestSkewsAverage(darkYs(centerIndex + whichStaffLine))
        }
      })
    val bestCenterY = darkYs(bestCenterIndex)

    Metrics(partiallyErased.w, bestSkew, waveLength, wavePhase, bestCenterY)
  }

  def annotateCenterY(input:GrayImage, metrics:Metrics) {
    val annotated = input.toColorImage
    val color = (255,0,0) // red
    (-2 to 2).foreach { staffLine =>
      for (x <- 0 until 20) {
        val y = metrics.centerY + (staffLine * metrics.waveLength)
        val skewedY = (y + (x * metrics.skew / annotated.w)).intValue
        if (skewedY >= 0 && skewedY < annotated.h)
          annotated(x, skewedY) = color
      }
    }
    annotated.saveTo(new File("center_y.png"))
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
//if (y > 15 && y < 20) {
          //println("segment: %s, prevSegment: %s".format(segment, previousSegment))
//}

            segment.x0 < previousSegment.x1 && segment.x1 > previousSegment.x0
          }.isDefined
        }
//if (y > 15 && y < 20) {
//println("Group for %s: %s".format(segment, touchingGroupIfAny))
//}
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
//if (y > 15 && y < 20) {
//println("----------")
//inactiveGroups.foreach { println }
//println("-")
//activeGroups.foreach { println }
//}
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

  def annotateBounds(baseImage:GrayImage, bounds:List[BoundingBox]) {
    val annotated = baseImage.toColorImage
    bounds.foreach { box =>
      (box.minX to box.maxX).foreach { x =>
        annotated(x, box.minY) = Colors.annotation
        annotated(x, box.maxY) = Colors.annotation
      }
      (box.minY to box.maxY).foreach { y =>
        annotated(box.minX, y) = Colors.annotation
        annotated(box.maxX, y) = Colors.annotation
      }
    }
    annotated.saveTo(new File("bounds.png"))
  }

  def annotateNotes(notes:Set[Note], excerpt:GrayImage, caseNum:Int) {
    val staffSeparation = 6
    val xSeparation = 18
    val darkYellow = (128, 128, 0)
    val brightYellow = (255, 255, 0)
    val staffHeight = 100
    val image = new ColorImage(excerpt.w, staffHeight + excerpt.h)

    // draw notes
    notes.foreach { note =>
      val centerY = (staffHeight / 2) +
        (note.staffY * staffSeparation / 2).intValue
      (-8 to 8).foreach { x =>
        (-8 to 8).foreach { y =>
          if ((x * x) + 2 * (y * y) < 20) {
            image((note.staffX + 1) * xSeparation + x, centerY + y) =
              brightYellow
          }
        }
      }

      def drawLedgerLine(staffY:Int) {
        val ledgerLineY = ((staffY / 2).intValue * 2 * staffSeparation / 2) +
          (staffHeight / 2)
        (-8 to 8).foreach { x =>
          image((note.staffX + 1) * xSeparation + x, ledgerLineY) = darkYellow
        }
      }

      // draw ledger lines
      var staffY = note.staffY
      while (staffY >= 6) {
        drawLedgerLine(staffY)
        staffY -= 2
      }
      while (staffY <= -6) {
        drawLedgerLine(staffY)
        staffY += 2
      }

      // draw staff (top-most so line vs. space notes are more obvious)
      (-4 to 4 by 2).foreach { staffY =>
        (0 until image.w).foreach { x =>
          image(x, (staffHeight / 2) + (staffY * staffSeparation / 2)) =
            darkYellow
        }
      }
    }

    // copy excerpt
    (0 to excerpt.h).foreach { y =>
      (0 to excerpt.w).foreach { x =>
        val v = excerpt(x, y)
        image(x, y + staffHeight) = (v, v, v)
      }
    }

    image.saveTo(new File("notes" + caseNum + ".png"))
  }

  def recognizeNotesFromBounds(bounds:List[BoundingBox], metrics:Metrics) = {
    val notSmallBounds = bounds.filter { bound =>
      bound.maxX - bound.minX > 4 && bound.maxY - bound.minY > 4
    }
    val notSmallBoundsSorted = notSmallBounds.sort { (bound1, bound2) =>
      val midX1 = (bound1.maxX + bound1.minX) / 2
      val midX2 = (bound2.maxX + bound2.minX) / 2
      midX1 < midX2
    }

    var lastNoteMidX = -99
    var staffX = -1
    var notes:List[Note] = Nil
    notSmallBoundsSorted.foreach { bound =>
      val midX = (bound.maxX + bound.minX) / 2
      val midY = (bound.maxY + bound.minY) / 2
      val unskewedY = midY + (midX * metrics.skew / metrics.width)
      val staffY = (unskewedY - metrics.centerY) / (metrics.waveLength / 2)
      if (Math.abs(midX - lastNoteMidX) >= 10)
        staffX += 1
        
      notes = Note(staffX, staffY.intValue) :: notes

      lastNoteMidX = midX
    }
    Set() ++ notes
  }

  def recognizeNotes(box:Annotation, caseNum:Int) = {
    val original = ColorImage.readFromFile(new File("photo.jpeg")).toGrayImage
    //val excerpt = original.crop(200, 50, 220, 75) // straight with notes
    //val excerpt = original.crop(540, 180, 60, 60) // diagonal down
    //val excerpt = original.crop(0, 55, 40, 90) // diagonal up
    val excerpt = original.crop(box.left, box.top, box.width, box.height)
    val (justNotes, partiallyErased) = separateNotes(excerpt)
    val metrics = estimateMetrics(partiallyErased)

    val segments = scanSegments(justNotes)
    val segmentGroups = groupTouchingSegments(segments)
    val bounds = boundSegmentGroups(segmentGroups)
    val estimatedNotes = recognizeNotesFromBounds(bounds, metrics)
    annotateNotes(estimatedNotes, excerpt, caseNum)
    estimatedNotes
  }

  def doRecognitionForEachBox {
    var globalPerformance = new Performance()
    val annotationsString:String =
      scala.io.Source.fromFile("boxes.json").mkString
    val annotationsJson:List[Map[String,Any]] = 
      Json.parse(annotationsString).asInstanceOf[List[Map[String,Any]]]
    var caseNum = 0
    annotationsJson.foreach { annotationJson =>
      val left = annotationJson("left").asInstanceOf[Int]
      val top = annotationJson("top").asInstanceOf[Int]
      val width = annotationJson("width").asInstanceOf[Int]
      val height = annotationJson("height").asInstanceOf[Int]

      var annotatedNotes:Set[Note] = Set()
      var numGroup = 0
      annotationJson("notes").asInstanceOf[List[List[Int]]].foreach { group =>
        annotatedNotes ++= group.map { staffY => Note(numGroup, staffY) }
        numGroup += 1
      }

      val annotation = Annotation(left, top, width, height, annotatedNotes)
      val estimatedNotes = recognizeNotes(annotation, caseNum)
      val performance = calcPerformance(estimatedNotes, annotatedNotes)
      println("Case num %d: precision: %.2f, recall: %.2f".format(
        caseNum, performance.precision, performance.recall))
      globalPerformance = new Performance(
        globalPerformance.numCorrect + performance.numCorrect,
        globalPerformance.numIncorrect + performance.numIncorrect,
        globalPerformance.numMissing + performance.numMissing)

      caseNum += 1
    }
    println("Total:      precision: %.2f -- recall: %.2f".format(
      globalPerformance.precision, globalPerformance.recall))
  }

  def calcPerformance(estimated:Set[Note], annotated:Set[Note]) = {
    var numCorrect = 0
    var numIncorrect = 0
    var numMissing = 0

    annotated.foreach { annotatedNote =>
      if (estimated.contains(annotatedNote))
        numCorrect += 1
      else
        numMissing += 1
    }
    estimated.foreach { estimatedNote =>
      if (!annotated.contains(estimatedNote))
        numIncorrect += 1
    }

    new Performance(numCorrect, numIncorrect, numMissing)
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      doRecognitionForEachBox
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
