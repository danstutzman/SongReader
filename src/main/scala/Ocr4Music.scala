import java.io.File
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.image.BufferedImage

class ColorImage(val w:Int, val h:Int, val data:Array[(Int,Int,Int)]) {
  def update(x:Int, y:Int, tuple:(Int,Int,Int)) {
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
    if (x < 0 || x >= w || y < 0 || y >= h) {
      0
    } else {
      data(y * w + x)
    }
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
      if (v < 0) { (0, 0, 255) } // blue (too cold)
      else if (v > 255) { (255, 0, 0) } // red (too hot)
      else { (v, v, v) }
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

object Ocr4Music {
  def main(args:Array[String]) {
    try {
      println("\u001b" + "[1;37m"); // make program output bright white
      tryLoadingAndSavingFiles
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

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

  def findArgmax(inputs:Range, block:Int=>Int) : Int = {
    var max = inputs(0)
    var argmax = block(inputs(0))
    inputs.foreach { input =>
      val output = block(input)
      if (output > max) {
        max = output
        argmax = input
      }
    }
    argmax
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
    val bestSkew = findArgmax((-maxSkew to maxSkew), { skew =>
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
      annotated(x, (image.h / 2) + skewAmount) = (255, 0, 0)
    }
    annotated
  }

  def tryLoadingAndSavingFiles {
    val colorImage = readColorImage(new File("photo.jpeg"))
    val grayImage = colorImage.toGrayImage
    //val excerpt = grayImage.crop(200, 50, 220, 75) // straight with notes
    //val excerpt = grayImage.crop(540, 180, 60, 60) // diagonal down
    val excerpt = grayImage.crop(0, 85, 40, 40) // diagonal up
    val whiteBackground = excerpt.brighten(130)
    val binaryNonStaff = whiteBackground.blurVertically1.binarize(200)
    val augmentedBinaryNonStaff = binaryNonStaff.blurVertically4.binarize(254)
    val partiallyErased = whiteBackground.addWithCeiling(
      augmentedBinaryNonStaff.inverse)
    partiallyErased.toColorImage.saveTo(new File("out.png"))
    val bestSkew = findBestSkew(partiallyErased)
    println("best skew: %d".format(bestSkew))
    val annotatedImage = annotateSkew(bestSkew, excerpt)
    annotatedImage.saveTo(new File("annotated.png"))

    val imageOfSkews = constructImageComparingSkews(partiallyErased)
    imageOfSkews.saveTo(new File("skews.png"))
  }
}
