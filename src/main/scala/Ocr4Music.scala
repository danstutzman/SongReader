import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val annotation = (255, 0, 0)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
}

case class Metrics(
  val skew:Int,
  val waveLength:Float,
  val wavePhase:Float,
  val centerY:Float
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

  def eraseNotes(input:GrayImage) : GrayImage = {
    val whiteBackground = input.brighten(130)
    val binaryNonStaff = whiteBackground.blurVertically1.binarize(200)
    val augmentedBinaryNonStaff = binaryNonStaff.blurVertically4.binarize(254)
    val partiallyErased = whiteBackground.addWithCeiling(
      augmentedBinaryNonStaff.inverse)
    partiallyErased
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

    Metrics(bestSkew, waveLength, wavePhase, bestCenterY)
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

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      recognizeNotes
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def recognizeNotes {
    val original = ColorImage.readFromFile(new File("photo.jpeg")).toGrayImage
    val excerpt = original.crop(200, 50, 220, 75) // straight with notes
    //val excerpt = original.crop(540, 180, 60, 60) // diagonal down
    //val excerpt = original.crop(0, 55, 40, 90) // diagonal up
    val partiallyErased = eraseNotes(excerpt)
    val metrics = estimateMetrics(partiallyErased)
  }
}
