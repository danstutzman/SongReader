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

class ColorImage(val w:Int, val h:Int, val data:Array[(Int,Int,Int)]) {
  def update(x:Int, y:Int, tuple:(Int,Int,Int)) {
    if (x >= 0 && x < w && y >= 0 && y < h)
      data(y * w + x) = tuple
  }
  def toGrayImage : GrayImage = {
    val newData = data.map { rgb => (rgb._1 + rgb._2 + rgb._3) / 3 }
    new GrayImage(w, h, newData)
  }
  def saveTo(file:File) {
    def convertRGBTupleToARGBInt(rgb:(Int,Int,Int)) : Int = {
      val (r, g, b) = rgb
      (255 << 24) + (r << 16) + (g << 8) + (b << 0)
    }
    val imageOut = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val pixelsARGBNew = data.map { convertRGBTupleToARGBInt }
    imageOut.setRGB(0, 0, w, h, pixelsARGBNew, 0, w)
    ImageIO.write(imageOut, "PNG", file)
  }
}

class GrayImage(val w:Int, val h:Int, val data:Array[Int]) {
  def this(w:Int, h:Int) = this(w, h, new Array[Int](w * h))
  def apply(x:Int, y:Int) = {
    if (x < 0 || x >= w || y < 0 || y >= h)
      0
    else
      data(y * w + x)
  }
  def update(x:Int, y:Int, brightness:Int) {
    data(y * w + x) = brightness
  }
  def +(scalar:Int) = { new GrayImage(w, h, data.map { _ + scalar }) }
  def -(scalar:Int) = { new GrayImage(w, h, data.map { _ - scalar }) }
  def *(scalar:Int) = { new GrayImage(w, h, data.map { _ * scalar }) }
  def /(scalar:Int) = { new GrayImage(w, h, data.map { _ / scalar }) }
  def crop(startX:Int, startY:Int, newW:Int, newH:Int) = {
    val newData = new Array[Int](newW * newH)
    for (y <- 0 until newH) {
      System.arraycopy(data, (startY + y) * w + startX,
        newData, y * newW, newW)
    }
    new GrayImage(newW, newH, newData)
  }
  def toColorImage : ColorImage = {
    new ColorImage(w, h, data.map { v =>
      if (v < 0)
        Colors.underflow
      else if (v > 255) 
        Colors.overflow
      else
        (v, v, v)
    })
  }
  def giveBrightnessPerPixel(block:(Int,Int)=>Int) = {
    val newData = new Array[Int](w * h)
    var i = 0
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        newData(i) = block(x, y)
        i += 1
      }
    }
    newData
  }
  def blurVertically1 = {
    new GrayImage(w, h, giveBrightnessPerPixel { (x, y) =>
      (this(x, y - 1) + this(x, y) + this(x, y + 1)) / 3
    })
  }
  def blurVertically4 = {
    new GrayImage(w, h, giveBrightnessPerPixel { (x, y) =>
      (
        this(x, y - 4) +
        this(x, y - 3) +
        this(x, y - 2) +
        this(x, y - 1) +
        this(x, y - 0) +
        this(x, y + 1) +
        this(x, y + 2) +
        this(x, y + 3) +
        this(x, y + 4)
      ) / 9
    })
  }
  def binarize(threshold:Int) = {
    new GrayImage(w, h, data.map { v =>
      if (v >= threshold) 255 else 0
    })
  }
  def brighten(threshold:Int) = {
    new GrayImage(w, h, data.map { v =>
      if (v >= threshold) 255 else v + (255 - threshold)
    })
  }
  def inverse = { new GrayImage(w, h, data.map { 255 - _ }) }
  def addWithCeiling(otherImage:GrayImage) = {
    new GrayImage(w, h, giveBrightnessPerPixel { (x, y) =>
      val newV = this(x, y) + otherImage(x, y)
      if (newV > 255) 255 else newV
    })
  }
  def saveTo(file:File) { this.toColorImage.saveTo(file) }
}

case class Metrics(
  val skew:Int,
  val waveLength:Float,
  val wavePhase:Float
) {}

object Ocr4Music {
  def readColorImage(file:File) : ColorImage = {
    def convertARGBIntToRGBTuple(argb:Int) : (Int,Int,Int) = {
      val a = (argb >> 24) & 0xff
      val r = (argb >> 16) & 0xff
      val g = (argb >> 8) & 0xff
      val b = (argb >> 0) & 0xff
      (r, g, b)
    }
    val unrotatedBufferedImage = ImageIO.read(file)

    def rotate90Degrees(imageIn:BufferedImage) : BufferedImage = {
      val imageOut = new BufferedImage(
        imageIn.getHeight(), imageIn.getWidth(),
        BufferedImage.TYPE_INT_ARGB)
      val g2d = imageOut.getGraphics().asInstanceOf[Graphics2D]
      g2d.rotate(Math.toRadians(90.0))
      g2d.drawImage(imageIn, 0, -imageIn.getHeight(), null)
      g2d.dispose()
      imageOut
    }
    val bufferedImage = rotate90Degrees(unrotatedBufferedImage)

    val w = bufferedImage.getWidth()
    val h = bufferedImage.getHeight()
    val pixelsARGB:Array[Int] = bufferedImage.getRGB(0, 0, w, h, null, 0, w)
    val data = pixelsARGB.map { convertARGBIntToRGBTuple }
    val colorImage = new ColorImage(w, h, data)
    colorImage
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

  def annotateSkew(skew:Int, image:GrayImage) : ColorImage = {
    var annotated = image.toColorImage
    for (x <- 0 until image.w) {
      val skewAmount = x * skew / image.w
      annotated(x, (image.h / 2) + skewAmount) = Colors.annotation
    }
    annotated
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

    Metrics(bestSkew, waveLength, wavePhase)
  }

  def tryLoadingAndSavingFiles {
    val original = readColorImage(new File("photo.jpeg")).toGrayImage
    //val excerpt = original.crop(200, 50, 220, 75) // straight with notes
    val excerpt = original.crop(540, 180, 60, 60) // diagonal down
    //val excerpt = original.crop(0, 85, 40, 40) // diagonal up
    val partiallyErased = eraseNotes(excerpt)
    val metrics = estimateMetrics(partiallyErased)
    println(metrics)
  }

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)
      tryLoadingAndSavingFiles
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
