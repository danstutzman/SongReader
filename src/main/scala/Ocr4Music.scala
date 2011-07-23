import java.io.File
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.image.BufferedImage

object Ocr4Music {
  def main(args: Array[String]) {
    try {
      println("\u001b" + "[1;37m"); // make program output bright white
      tryLoadingSavingFiles
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def loadImageFile(pathname:String) = {
    val file = new File(pathname)
    ImageIO.read(file)
  }

  def rotate90Degrees(imageIn:BufferedImage) = {
    val imageOut = new BufferedImage(
      imageIn.getHeight(), imageIn.getWidth(),
      BufferedImage.TYPE_INT_ARGB)
    val g2d = imageOut.getGraphics().asInstanceOf[Graphics2D]
    g2d.rotate(Math.toRadians(90.0))
    g2d.drawImage(imageIn, 0, -imageIn.getHeight(), null)
    g2d.dispose()
    imageOut
  }

  def tryLoadingSavingFiles {
    val image = rotate90Degrees(loadImageFile("photo.jpeg"))
    val argb = image.getRGB(1, 1)
    var (a, r, g, b) = (
      0xff & (argb>>24),
      0xff & (argb>>16),
      0xff & (argb>>8),
      0xff & argb
    )
    r = 255
    g = 0
    b = 255
    val argb_new = (a << 24) + (r << 16) + (g << 8) + (b << 0)

    for (x <- 0 until 100) {
      for (y <- 0 until 100) {
        image.setRGB(x, y, argb_new)
      }
    }
    ImageIO.write(image, "PNG", new File("new2.png"))
  }
}
