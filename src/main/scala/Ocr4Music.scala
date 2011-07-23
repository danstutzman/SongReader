import java.io.File
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.image.BufferedImage

class ColorImage(_w:Int, _h:Int, _data:Array[(Int,Int,Int)]) {
  val w = _w
  val h = _h
  val data = _data
  def update(x:Int, y:Int, tuple:(Int,Int,Int)) {
    data(y * w + x) = tuple
  }
}

class GrayImage(_w:Int, _h:Int, _data:Array[Int]) {
  val w = _w
  val h = _h
  val data = _data
  def update(x:Int, y:Int, brightness:Int) {
    data(y * w + x) = brightness
  }
  def +(scalar:Int) = { new GrayImage(w, h, data.map { _ + scalar }) }
  def -(scalar:Int) = { new GrayImage(w, h, data.map { _ - scalar }) }
  def *(scalar:Int) = { new GrayImage(w, h, data.map { _ * scalar }) }
  def /(scalar:Int) = { new GrayImage(w, h, data.map { _ / scalar }) }
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

  def convertARGBIntToRGBTuple(argb:Int) : (Int,Int,Int) = {
    val a = (argb >> 24) & 0xff
    val r = (argb >> 16) & 0xff
    val g = (argb >> 8) & 0xff
    val b = (argb >> 0) & 0xff
    (r, g, b)
  }

  def convertRGBTupleToARGBInt(rgb:(Int,Int,Int)) : Int = {
    val (r, g, b) = rgb
    (255 << 24) + (r << 16) + (g << 8) + (b << 0)
  }

  def readColorImage(file:File) : ColorImage = {
    val unrotatedBufferedImage = ImageIO.read(file)
    val bufferedImage = rotate90Degrees(unrotatedBufferedImage)
    val w = bufferedImage.getWidth()
    val h = bufferedImage.getHeight()
    val pixelsARGB:Array[Int] = bufferedImage.getRGB(0, 0, w, h, null, 0, w)
    val data = pixelsARGB.map { convertARGBIntToRGBTuple }
    val colorImage = new ColorImage(w, h, data)
    colorImage
  }

  def writeColorImage(colorImage:ColorImage, file:File) {
    val (w, h) = (colorImage.w, colorImage.h)
    val imageOut = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val pixelsARGBNew = colorImage.data.map { convertRGBTupleToARGBInt }
    imageOut.setRGB(0, 0, w, h, pixelsARGBNew, 0, w)
    ImageIO.write(imageOut, "PNG", file)
  }

  def convertColorImageToGrayImage(colorImage:ColorImage) : GrayImage = {
    val data = colorImage.data.map { rgb => (rgb._1 + rgb._2 + rgb._3) / 3 }
    new GrayImage(colorImage.w, colorImage.h, data)
  }

  def convertGrayImageToColorImage(grayImage:GrayImage) : ColorImage = {
    val data = grayImage.data.map { v => (v, v, v) }
    new ColorImage(grayImage.w, grayImage.h, data)
  }

  def tryLoadingAndSavingFiles {
    val colorImage = readColorImage(new File("photo.jpeg"))
    val grayImage = convertColorImageToGrayImage(colorImage)
    val grayImage2 = (grayImage * -1) + 255
    val colorImage2 = convertGrayImageToColorImage(grayImage2)

    for (x <- 0 until 100) {
      for (y <- 0 until 100) {
        colorImage2(x, y) = (x, y, 0)
      }
    }

    writeColorImage(colorImage2, new File("out.png"))
  }
}
