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
  def crop(startX:Int, startY:Int, newW:Int, newH:Int) = {
    val newData = new Array[Int](newW * newH)
    for (y <- 0 until newH) {
      System.arraycopy(data, (startY + y) * w + startX,
        newData, y * newW, newW)
    }
    new GrayImage(newW, newH, newData)
  }
  def toColorImage : ColorImage = {
    val newData = data.map { v => (v, v, v) }
    new ColorImage(w, h, newData)
  }
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

  def tryLoadingAndSavingFiles {
    val colorImage = readColorImage(new File("photo.jpeg"))
    val grayImage = colorImage.toGrayImage
    val excerpt = grayImage.crop(200, 50, 220, 75)
    excerpt.toColorImage.saveTo(new File("out.png"))
  }
}
