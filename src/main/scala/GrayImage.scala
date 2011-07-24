import java.io.File

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
