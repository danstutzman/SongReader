case class VLine (
  val xIntercept:Int,
  val y0:Int,
  val y1:Int
) {
  def toMap() : Map[String,Int] = {
    Map("xIntercept" -> xIntercept, "y0" -> y0, "y1" -> y1)
  }
}
object VLine {
  def fromMap(map:Map[String,Int]) : VLine = {
    VLine(map("xIntercept"), map("y0"), map("y1"))
  }
}
