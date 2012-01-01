import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import org.apache.sanselan.Sanselan
import org.apache.sanselan.common.IImageMetadata
import org.apache.sanselan.formats.jpeg.JpegImageMetadata
import org.apache.sanselan.formats.tiff.TiffField
import org.apache.sanselan.formats.tiff.TiffImageMetadata
import org.apache.sanselan.formats.tiff.constants.TagInfo

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
  def scaleValueToMax255 = {
    // init with 1 not 0 to avoid divide by zero if all values are zero
    var maxR = 1
    var maxG = 1
    var maxB = 1

    (0 until h).foreach { y =>
      (0 until w).foreach { x =>
        val (r, g, b) = this(x, y)
        if (r > maxR) maxR = r
        if (g > maxG) maxG = g
        if (b > maxB) maxB = b
      }
    }
    ColorImage.giveRGBPerPixel(w, h) { (x, y) =>
      val (r, g, b) = this(x, y)
      (r * 255 / maxR, g * 255 / maxG, b * 255 / maxB)
    }
  }
  def saveTo(file:File) {
    def convertRGBTupleToRGBInt(rgb:(Int,Int,Int)) : Int = {
      val (r, g, b) = rgb
      (0 << 24) + (r << 16) + (g << 8) + (b << 0)
    }
    val imageOut = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val pixelsRGBNew = data.map { convertRGBTupleToRGBInt }
    imageOut.setRGB(0, 0, w, h, pixelsRGBNew, 0, w)

    if (file.getName().endsWith(".png"))
      ImageIO.write(imageOut, "PNG", file)
    else if (file.getName().endsWith(".jpg"))
      ImageIO.write(imageOut, "JPEG", file)
    else if (file.getName().endsWith(".jpeg"))
      ImageIO.write(imageOut, "JPEG", file)
    else
      throw new RuntimeException(
        "Don't know what format %s is".format(file.getName()))
  }
}

object ColorImage {
  def allocateBlankData(length:Int) = {
    val data = new Array[(Int,Int,Int)](length)
    (0 until length).foreach { i => data(i) = (0,0,0) }
    data
  }
  def findRotationNeeded(file:File) : Int = {
    Sanselan.getMetadata(file) match {
      case jpegMetadata:JpegImageMetadata =>
        val items = jpegMetadata.getItems()
        (0 until items.size()).foreach { i =>
          items.get(i) match {
            case item:TiffImageMetadata.Item =>
              if (item.getTiffField.getTagName == "Orientation") {
                return item.getTiffField.getValueDescription match {
                  case "1" => 0
                  case "6" => 90
                  case "3" => 80
                  case "8" => 270
                  case   _ => 0
                }
              }
            case _ => ()
          }
        }
        case _ => {}
    }
    return 0
  }
  def rotateBufferedImage(unrotatedImage:BufferedImage, rotation:Int) = {
    rotation match {
      case 0 =>
        unrotatedImage
      case 90 =>
        val imageOut = new BufferedImage(
          unrotatedImage.getHeight(), unrotatedImage.getWidth(),
          BufferedImage.TYPE_INT_ARGB)
        val g2d = imageOut.getGraphics().asInstanceOf[Graphics2D]
        g2d.rotate(Math.toRadians(90.0))
        g2d.drawImage(unrotatedImage, 0, -unrotatedImage.getHeight(), null)
        g2d.dispose()
        imageOut
      case 270 =>
        val imageOut = new BufferedImage(
          unrotatedImage.getHeight(), unrotatedImage.getWidth(),
          BufferedImage.TYPE_INT_ARGB)
        val g2d = imageOut.getGraphics().asInstanceOf[Graphics2D]
        g2d.rotate(Math.toRadians(270.0))
        g2d.drawImage(unrotatedImage, -unrotatedImage.getWidth(), 0, null)
        g2d.dispose()
        imageOut
      case 180 =>
        val imageOut = new BufferedImage(
          unrotatedImage.getWidth(), unrotatedImage.getHeight(),
          BufferedImage.TYPE_INT_ARGB)
        val g2d = imageOut.getGraphics().asInstanceOf[Graphics2D]
        g2d.rotate(Math.toRadians(180.0))
        g2d.drawImage(unrotatedImage,
          -unrotatedImage.getWidth(), -unrotatedImage.getHeight(), null)
        g2d.dispose()
        imageOut
    }
  }
  def readFromFile(file:File) : ColorImage = {
    def convertARGBIntToRGBTuple(argb:Int) : (Int,Int,Int) = {
      val a = (argb >> 24) & 0xff
      val r = (argb >> 16) & 0xff
      val g = (argb >> 8) & 0xff
      val b = (argb >> 0) & 0xff
      (r, g, b)
    }
    val unrotatedImage = ImageIO.read(file)
    val rotation = findRotationNeeded(file)
    val bufferedImage = rotateBufferedImage(unrotatedImage, rotation)
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
