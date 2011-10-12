case class Beam (
  val x0:Int,
  val x1:Int,
  val y0:Int,
  val y1:Int
) {
  def toMap() : Map[String,Int] = {
    Map("x0" -> x0, "x1" -> x1, "y0" -> y0, "y1" -> y1)
  }
}
object Beam {
  def fromMap(map:Map[String,Int]) : Beam = {
    Beam(map("x0"), map("x1"), map("y0"), map("y1"))
  }
}
