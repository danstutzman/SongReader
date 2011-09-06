import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

class ColorImage(val w:Int, val h:Int, val data:Array[(Int,Int,Int)]) {
  def this(w:Int, h:Int) = this(w, h, ColorImage.allocateBlankData(w * h))
  def copy : ColorImage = { new ColorImage(w, h, data.clone) }
  def apply(x:Int, y:Int) : (Int,Int,Int) = {
    if (x < 0 || x >= w || y < 0 || y >= h)
      (0, 0, 0)
    else
      data(y * w + x)
  }
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

object ColorImage {
  def allocateBlankData(length:Int) = {
    val data = new Array[(Int,Int,Int)](length)
    (0 until length).foreach { i => data(i) = (0,0,0) }
    data
  }
  def readFromFile(file:File) : ColorImage = {
    def convertARGBIntToRGBTuple(argb:Int) : (Int,Int,Int) = {
      val a = (argb >> 24) & 0xff
      val r = (argb >> 16) & 0xff
      val g = (argb >> 8) & 0xff
      val b = (argb >> 0) & 0xff
      (r, g, b)
    }
    val bufferedImage = ImageIO.read(file)
    val w = bufferedImage.getWidth()
    val h = bufferedImage.getHeight()
    val pixelsARGB:Array[Int] = bufferedImage.getRGB(0, 0, w, h, null, 0, w)
    val data = pixelsARGB.map { convertARGBIntToRGBTuple }
    val colorImage = new ColorImage(w, h, data)
    colorImage
  }
  def giveRGBPerPixel(w:Int, h:Int)(block:(Int,Int)=>(Int,Int,Int)) = {
    val newData = new Array[(Int,Int,Int)](w * h)
    var i = 0
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        newData(i) = block(x, y)
        i += 1
      }
    }
    new ColorImage(w, h, newData)
  }
}
