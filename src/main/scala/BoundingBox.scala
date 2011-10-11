case class BoundingBox (
  val minX:Int,
  val maxX:Int,
  val minY:Int,
  val maxY:Int
) {
  def toMap() : Map[String,Int] = {
    Map("minX" -> minX, "maxX" -> maxX, "minY" -> minY, "maxY" -> maxY)
  }
}
object BoundingBox {
  def fromMap(map:Map[String,Int]) : BoundingBox = {
    BoundingBox(map("minX"), map("maxX"), map("minY"), map("maxY"))
  }
}
