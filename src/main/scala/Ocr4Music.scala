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

case class LabeledBoundingBox (
  val label:BoundingBoxLabel,
  val box:BoundingBox
) {}

sealed abstract class BoundingBoxLabel
case object Note extends BoundingBoxLabel
case object NonNote extends BoundingBoxLabel

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

case class LabeledPoint (
  val label:String,
  val x:Int,
  val y:Int
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

  def maxVerticalSkewGivenWidth(width:Int) : Int = { width / 4 }

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

  def newStaffTracking(input:GrayImage) {
    val sliceWidth = 10
    val skewDemo = input.copy
    var y = input.h / 2.0
    (0 until input.w by sliceWidth).foreach { x0 =>
      if (x0 + sliceWidth < input.w) {
        // If the next slice will be really narrow,
        // merge it with the 2nd to last, then skip the last one
        val x1 =
          if (x0 + sliceWidth * 2 >= input.w) input.w
          else x0 + sliceWidth
        val bestSkew = findBestSkew(input, x0, x1)
        val bestSkewsAverage = averageOfSkewedImage(input, bestSkew, x0, x1)
        (x0 until x1).foreach { x =>
          y += bestSkew.floatValue / input.w
          skewDemo(x, y.intValue) = 0
        }
        (-4 to 4).foreach { deltaY =>
          skewDemo(x1 - 1, y.intValue + deltaY) = 0
        }
      }
    }
    //skewDemo.saveTo(new File("skew_demo.png"))

    (0 until input.w).foreach { x =>
      val y = 0.0012 * x * x + 20
      val yInt = y.intValue
      if (yInt >= 0 && yInt < input.h)
        skewDemo(x, yInt) = 0
    }
    skewDemo.saveTo(new File("skew_demo.png"))

/*
    val houghTransform = new GrayImage(200, 200)
    (0 until input.w).foreach { x =>
      (0 until 100).foreach { y =>
        val v = 255 - input(x, y) //if (input(x, y) < 230) 1 else 0
        (-100 until 100).foreach { mCents =>
          val yIntercept = y - (mCents * 0.01) * x
          val imageY = ((yIntercept * 1) + 100).intValue
          if (imageY >= 0 && imageY < houghTransform.h) {
            houghTransform((mCents + 100) / 1, imageY / 1) =
              houghTransform((mCents + 100) / 1, imageY / 1) + v
          }
        }
      }
    }
    val max = houghTransform.data.max 
    (0 until houghTransform.w).foreach { x =>
      (0 until houghTransform.h).foreach { y =>
        houghTransform(x, y) = (houghTransform(x, y) * (250.0 / max)).intValue
      }
    }
    houghTransform.saveTo(new File("hough_transform.png"))
*/
/*
    val houghTransform2 = new GrayImage(200, 200)
    (0 until input.w).foreach { x =>
      (0 until 100).foreach { y =>
        val v = 255 - input(x, y) //if (input(x, y) < 230) 1 else 0
        (-100 until 100).foreach { aTenThousandths =>
          val yIntercept = y - (aTenThousandths * 0.00003) * x * x
          // assume b term (for b*x) is 0
          val imageY = ((yIntercept / 1.0) + 100).intValue
          if (imageY >= 0 && imageY < houghTransform2.h) {
            houghTransform2(aTenThousandths + 100, imageY) =
              houghTransform2(aTenThousandths + 100, imageY) + v
          }
        }
      }
    }
    val max = houghTransform2.data.max 
    (0 until houghTransform2.w).foreach { x =>
      (0 until houghTransform2.h).foreach { y =>
        houghTransform2(x, y) = (houghTransform2(x, y) * (250.0 / max)).intValue
      }
    }
    houghTransform2.saveTo(new File("hough_transform2.png"))
*/
    val coverageDemo = input.copy
    val params = QuadraticParameterSearch(
      //ParameterSearch(-0.01f, 0.01f, 0.001f),
      //ParameterSearch(-1.0f, 1.0f, 0.1f),
      //ParameterSearch(-100.0f, 100.0f, 1.0f))
      ParameterSearch(-0.001f, 0.001f, 0.0001f),
      ParameterSearch(-1.0f, 1.01f, 0.01f),
      ParameterSearch(-100.0f, 100.0f, 1.0f))
    val halfW = coverageDemo.w / 2
/*
    var a = params.a.min
    while (a <= params.a.max) {
      var b = params.b.min
      while (b <= params.b.max) {
        (-halfW until halfW).foreach { x =>
          val y = (a * x * x + b * x).intValue + (input.h / 2)
          if (y >= 0 && y < coverageDemo.h)
            coverageDemo(x + halfW, y) = 0
        }
        b += params.b.step
      }
      a += params.a.step
    }
    coverageDemo.saveTo(new File("coverage_demo.png"))
*/

    val numASteps =
      Math.ceil((params.a.max - params.a.min) / params.a.step).intValue + 1
    val numBSteps =
      Math.ceil((params.b.max - params.b.min) / params.b.step).intValue + 1
    val numCSteps =
      Math.ceil((params.c.max - params.c.min) / params.c.step).intValue + 1
    val hough = new Array[Int](numASteps * numBSteps * numCSteps)
    //val hough2 = new GrayImage(numBSteps, numCSteps)
    var maxTwo = 0
    var bestBTwo = 0.0
    var bestCTwo = 0.0
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = 255 - input(x, y)
        val xCentered = x - input.w / 2
        val yCentered = y - input.h / 2

        val halfW = coverageDemo.w / 2
        var a = params.a.min
        var aSteps = 0
        while (a <= params.a.max) {
          var b = params.b.min
          var bSteps = 0
          while (b <= params.b.max) {
            val cSolved =
              yCentered - (a * xCentered * xCentered) - (b * xCentered)
            val cSteps = ((cSolved - params.c.min) / params.c.step).intValue
            if (cSteps >= 0 && cSteps < numCSteps) {
              hough(aSteps * numBSteps * numCSteps +
                                bSteps * numCSteps +
                                            cSteps) += v
              //hough2(bSteps, cSteps) = hough2(bSteps, cSteps) + v
              //if (hough2(bSteps, cSteps) > maxTwo) {
              //  maxTwo = hough2(bSteps, cSteps)
              //  bestBTwo = b
              //  bestCTwo = cSolved
              //}
            }
            b += params.b.step
            bSteps += 1
          }
          a += params.a.step
          aSteps += 1
        }

      }
    }

    //val max2 = hough2.data.max 
    //(0 until hough2.w).foreach { x =>
    //  (0 until hough2.h).foreach { y =>
    //    hough2(x, y) = (hough2(x, y) * (250.0 / max2)).intValue
    //  }
    //}
    //hough2.saveTo(new File("hough2.png"))

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

    var slice = new Array[Int](numCSteps)
    var walkingCSteps = 0
    var walkingC = params.c.min
    while (walkingC <= params.c.max) {
      slice(walkingCSteps) = hough(bestASteps * numBSteps * numCSteps +
                                               bestBSteps * numCSteps +
                                                        walkingCSteps)
      walkingC += params.c.step
      walkingCSteps += 1
    }

    val staffSeparationAsCUnits = 7.0f
    var bestSum = 0.0
    var bestCenterCSteps = 0
    (0 until slice.length).foreach { centerCSteps =>
      var sum = 0.0
      var synthetic = synthesizeTeethGraph(
        centerCSteps, staffSeparationAsCUnits, slice.length)
      (0 until slice.length).foreach { c =>
        sum += slice(c) * -synthetic(c)
      }
      if (sum > bestSum) {
        bestSum = sum
        bestCenterCSteps = centerCSteps
      }
    }
    val bestCenterC =
      Math.round(bestCenterCSteps * params.c.step + params.c.min).intValue
    println("bestC", bestC)
    println("bestCenterCSteps", bestCenterCSteps)
    println("bestCenterC", bestCenterC)

    val graph = new GrayImage(50, slice.length)
    (0 until slice.length).foreach { c =>
      val v = slice(c)
      (0 until 10).foreach { x => graph(x, c) = v / 30 }
    }
    graph.saveTo(new File("c_graph.png"))

    (-halfW until halfW).foreach { x =>
      (-2 to 2).foreach { staffY =>
        val y = Math.round(
          (bestA * x * x + bestB * x + bestCenterC) +
          (staffY * staffSeparationAsCUnits) + (input.h / 2)).intValue
        if (y >= 0 && y < coverageDemo.h)
          coverageDemo(x + halfW, y) = 0
      }
    }
    coverageDemo.saveTo(new File("coverage_demo.png"))

/*
    var alignDemo = input.toColorImage
    val staffSeparation = 7.0f
    (0 until input.w).foreach { x =>
      val slice = (0 until input.h).map { y => input(x, y) }.toList
      val sorted = slice.sort((x, y) => x > y)
      val median = sorted(input.h / 2)

      (0 until input.h).foreach { centerY =>
        var sum = 0.0
        var synthetic = synthesizeTeethGraph(centerY, staffSeparation, input.h)
        (0 until input.h).foreach { y =>
          sum += (input(x, y) - median) * synthetic(y)
        }
         
        alignDemo(x, centerY) = (0, 0, (sum / 3).intValue + 3)
      }
    }
    alignDemo.saveTo(new File("align_demo.png"))

    var alignDemo2 = alignDemo.copy
    (0 until input.w / 2).foreach { x =>
      (0 until input.h).foreach { y =>
        var maxY = 0
        (-7 to 7).foreach { y2 =>
          if (y + y2 >= 0 && y + y2 < input.h && alignDemo(x, y + y2)._3 > maxY) {
            maxY = alignDemo(x, y + y2)._3
println(maxY)
          }
        }
        alignDemo2(x, y) = (0, 0, maxY)
      }
    }
    alignDemo2.saveTo(new File("align_demo2.png"))
*/
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
    val bestSkew = findBestSkew(partiallyErased, 0, partiallyErased.w)
    val bestSkewsAverage = averageOfSkewedImage(
      partiallyErased, bestSkew, 0, partiallyErased.w)

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

  def annotateCenterY(input:GrayImage, metrics:Metrics) : ColorImage = {
    val annotated = input.toColorImage
    val color = (255,255,255) // white
    (-2 to 2).foreach { staffLine =>
      for (x1 <- 0 until input.w by 50) {
        for (x2 <- 0 until 5) {
          val x = x1 + x2 // for dashed line
          val y = metrics.centerY + (staffLine * metrics.waveLength)
          val skewedY = (y + (x * metrics.skew / annotated.w)).intValue
          if (skewedY >= 0 && skewedY < annotated.h)
            annotated(x, skewedY) = color
        }
      }
    }
    //annotated.saveTo(new File("center_y.png"))
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

  def annotateNotes(notes:Set[Note], excerpt:ColorImage, caseNum:Int) {
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
        image(x, y + staffHeight) = excerpt(x, y)
      }
    }

    image.saveTo(new File("notes" + caseNum + ".png"))
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

  def recognizeNotesFromBounds(
      bounds:List[LabeledBoundingBox], metrics:Metrics) = {
    val boundsSorted = bounds.sort { (bound1, bound2) =>
      val midX1 = (bound1.box.maxX + bound1.box.minX) / 2
      val midX2 = (bound2.box.maxX + bound2.box.minX) / 2
      midX1 < midX2
    }

    var lastNoteMidX = -99
    var staffX = -1
    var notes:List[Note] = Nil
    boundsSorted.foreach { bound =>
      

      val midX = (bound.box.maxX + bound.box.minX) / 2
      val midY = (bound.box.maxY + bound.box.minY) / 2
      val unskewedY = midY - (midX * metrics.skew / metrics.width)
      val staffY = (unskewedY - metrics.centerY) / (metrics.waveLength / 2)
      if (Math.abs(midX - lastNoteMidX) >= 10)
        staffX += 1
        
      // if note seems to be 0.3 or further, we're probably guessing wrong
      if (Math.abs(staffY - Math.round(staffY)) < 0.3)
        notes = Note(staffX, Math.round(staffY)) :: notes

      lastNoteMidX = midX
    }
    Set() ++ notes
  }

  def labelBounds(bounds:List[BoundingBox]) = {
    bounds.map { bound =>
      if (bound.maxX - bound.minX > 8 && bound.maxY - bound.minY > 6)
        LabeledBoundingBox(Note, bound)
      else
        LabeledBoundingBox(NonNote, bound)
    }
  }

  def recognizeNotes(original:GrayImage, box:Annotation,
      points:List[LabeledPoint], caseNum:Int) = {
    //val excerpt = original.crop(200, 50, 220, 75) // straight with notes
    //val excerpt = original.crop(540, 180, 60, 60) // diagonal down
    //val excerpt = original.crop(0, 55, 40, 90) // diagonal up
    val excerpt = original.crop(box.left, box.top, box.width, box.height)
    val (justNotes, partiallyErased) = separateNotes(excerpt)
newStaffTracking(partiallyErased)
    val metrics = estimateMetrics(partiallyErased)

    //excerptLabeledPointsContext(original, caseNum, points, metrics)
    val segments = scanSegments(justNotes)
    val segmentGroups = groupTouchingSegments(segments)
    val bounds = boundSegmentGroups(segmentGroups)
    val labeledBounds = labelBounds(bounds)
    val combinedBounds = combineNoteBounds(labeledBounds)
    val estimatedNotes = recognizeNotesFromBounds(combinedBounds, metrics)
    annotateNotes(estimatedNotes,
      annotateBounds(annotateCenterY(excerpt, metrics), labeledBounds), caseNum)
    estimatedNotes
    //Set[Note]()
  }

  def doRecognitionForEachBox {
    var globalPerformance = new Performance()
    val annotationsString:String =
      scala.io.Source.fromFile("boxes.json").mkString
    val annotationsJson:List[Map[String,Any]] = 
      Json.parse(annotationsString).asInstanceOf[List[Map[String,Any]]]
    var caseNum = 0
    val original = ColorImage.readFromFile(new File("photo.jpeg")).toGrayImage
    annotationsJson.foreach { annotationJson =>
if (caseNum == 4) {
      val left = annotationJson("left").asInstanceOf[Int]
      val top = annotationJson("top").asInstanceOf[Int]
      val width = annotationJson("width").asInstanceOf[Int]
      val height = annotationJson("height").asInstanceOf[Int]
      val points =
        annotationJson("points").asInstanceOf[List[Map[String,Any]]].map {
          pointJson => LabeledPoint(
            pointJson("type").asInstanceOf[String],
            pointJson("x").asInstanceOf[Int],
            pointJson("y").asInstanceOf[Int]
          )
        }

      var annotatedNotes:Set[Note] = Set()
      var numGroup = 0
      annotationJson("notes").asInstanceOf[List[List[Int]]].foreach { group =>
        annotatedNotes ++= group.map { staffY => Note(numGroup, staffY) }
        numGroup += 1
      }

      val annotation = Annotation(left, top, width, height, annotatedNotes)
      val estimatedNotes = recognizeNotes(original, annotation, points, caseNum)
      val performance = calcPerformance(estimatedNotes, annotatedNotes)
      println("Case num %d: precision: %.2f, recall: %.2f".format(
        caseNum, performance.precision, performance.recall))
      globalPerformance = new Performance(
        globalPerformance.numCorrect + performance.numCorrect,
        globalPerformance.numIncorrect + performance.numIncorrect,
        globalPerformance.numMissing + performance.numMissing)
}

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

  def excerptLabeledPointsContext(original:GrayImage, caseNum:Int,
      points:List[LabeledPoint], metrics:Metrics) {
    var i = 0
    points.foreach { point =>
      val excerptRadius = (metrics.waveLength * 3).intValue
      val excerpt = original.crop(
        point.x + 2 - excerptRadius, point.y + 5 - excerptRadius,
        excerptRadius * 2, excerptRadius * 2)
      val resized =
        excerpt.resize(100, 100, metrics.skew / metrics.width.floatValue)
      val filename = "points/%s.%d.%d.png".format(point.label, caseNum, i)
      resized.saveTo(new File(filename))

      i += 1
    }
  }

  def classifyNotesVsNonNotes {
    val whitePixelToNumNotes = new Array[Int](100 * 100)
    val whitePixelToNumNonNotes = new Array[Int](100 * 100)
    val blackPixelToNumNotes = new Array[Int](100 * 100)
    val blackPixelToNumNonNotes = new Array[Int](100 * 100)
    var numNoteImages = 0
    var numNonNoteImages = 0
    var i = 0
    new File("points").listFiles.foreach { file =>
      val image = ColorImage.readFromFile(file).toGrayImage
      val isNote = "^[LS]".r.findFirstMatchIn(file.getName).isDefined
      if (isNote) {
        (0 until (100 * 100)).foreach { i =>
          val isBlack = (image.data(i) > 110)
          if (isBlack)
            blackPixelToNumNotes(i) += 1
          else
            whitePixelToNumNotes(i) += 1
        }
        numNoteImages += 1
      } else {
        (0 until (100 * 100)).foreach { i =>
          val isBlack = (image.data(i) > 110)
          if (isBlack)
            blackPixelToNumNonNotes(i) += 1
          else
            whitePixelToNumNonNotes(i) += 1
        }
        numNonNoteImages += 1
      }

      val binarized = image.binarize(110)
      binarized.saveTo(new File("binarized%d.png".format(i)))
      i += 1
    }
    val notePrior = Math.log(numNoteImages.floatValue / (numNoteImages + numNonNoteImages))
    val nonNotePrior = Math.log(numNonNoteImages.floatValue / (numNoteImages + numNonNoteImages))

    new File("points").listFiles.foreach { file =>
      val image = ColorImage.readFromFile(file).toGrayImage
      var isNoteProb = 0.0
      var isNonNoteProb = 0.0
      (0 until (100 * 100)).foreach { i =>
        val isBlack = (image.data(i) > 110)
        if (isBlack) {
          isNoteProb += Math.log((blackPixelToNumNotes(i) + 0.1) /
            (blackPixelToNumNotes(i) + whitePixelToNumNotes(i)))
          isNonNoteProb += Math.log((blackPixelToNumNonNotes(i) + 0.1) /
            (blackPixelToNumNonNotes(i) + whitePixelToNumNonNotes(i)))
        } else {
          isNoteProb += Math.log((whitePixelToNumNotes(i) + 0.1) /
            (blackPixelToNumNotes(i) + whitePixelToNumNotes(i)))
          isNonNoteProb += Math.log((whitePixelToNumNonNotes(i) + 0.1) /
            (blackPixelToNumNonNotes(i) + whitePixelToNumNonNotes(i)))
        }
      }
      println("%-20s note:%f non-note:%f %s".format(
        file, isNoteProb, isNonNoteProb,
        if (isNoteProb > isNonNoteProb) "YES" else "NO"))
    }
  }

  //  ''''\  /\  /\  /\  /\  /''''   ^ +0.0 is max output
  //       \/  \/  \/  \/  \/        v -1.0 is min output
  //                | <= centerY
  //       [----] = staffSeparation
  def synthesizeTeethGraph(centerY:Float, staffSeparation:Float, h:Int) = {
    val y0 = centerY - (staffSeparation * 2.5)
    val y1 = centerY + (staffSeparation * 2.5)
    (0 until h).map { y =>
      if (y < y0) 0.0
      else if (y > y1) 0.0
      else {
        // ranges from 0.0 to 5.0
        val quotient = ((y - y0) / staffSeparation)

        // ranges from -0.5 at valleys to 0.5 at peaks
        val centeredSawtooth = (quotient - Math.floor(quotient)) - 0.5

        // ranges from 0.0 at valleys to 0.5 at peaks
        val centeredW = Math.abs(centeredSawtooth)

        (centeredW * 2.0) - 1.0
      }
    }.toList
  }

  def alignStaffToSlice {
    val original = ColorImage.readFromFile(new File("photo.jpeg")).toGrayImage
    val h = 108
    val slice = (38 until (58 + h)).map { y =>
      original(414, y)
    }.toList
    val sorted = slice.sort((x, y) => x > y)
    val median = sorted(h / 2)

    val graph = new ColorImage(300, h)
    (0 until slice.length).foreach { y =>
      (0 until slice(y) / 4).foreach { x =>
        graph(x, y) = (255, 255, 255)
      }
    }
    (0 until h).foreach { y =>
      graph(median / 4, y) = (255, 0, 0)
    }


    val syntheticLine = 60
    val synthetic = synthesizeTeethGraph(47, 7, h)
    (0 until h).foreach { y =>
      graph(syntheticLine, y) = (255, 0, 0)
    }
    (0 until h).foreach { y =>
      graph(syntheticLine + (synthetic(y) * 4).intValue, y) = (255, 255, 255)
    }

    val productLine = 120
    var bestSum = 0.0
    var bestCenterY = 0.0
    var bestStaffSeparation = 0.0
    (20 until 70).foreach { centerY =>
      var staffSeparation = 6.0f
      while (staffSeparation <= 8.0f) {
        var sum = 0.0
        var synthetic = synthesizeTeethGraph(centerY, staffSeparation, h)
        (0 until h).foreach { y =>
          sum += (slice(y) - median) * synthetic(y)
        }
        if (sum > bestSum) {
          bestSum = sum
          bestCenterY = centerY
          bestStaffSeparation = staffSeparation
        }
        graph((staffSeparation * 10).intValue + 30, centerY) =
          (0, 0, (sum / 5).intValue)
        staffSeparation += 0.1f
      }
    }

    def horizontalLine(centerX:Int, y:Double) {
      (-5 to 5).foreach { x =>
         graph(centerX + x, y.intValue) = (0, 0, 255)
      }
    }
    (-2 to 2).foreach { i =>
      horizontalLine(40, bestCenterY - (bestStaffSeparation * i))
    }

    graph.saveTo(new File("graph.png"))
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      doRecognitionForEachBox
      //alignStaffToSlice
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
