case class TemplateMatch (
  val x:Int,
  val y:Int,
  val w:Int,
  val h:Int,
  val staffY:Int,
  val templateName:String,
  val score:Int
) {
  def toMap() : Map[String,Any] = {
    Map("x" -> x, "y" -> y, "w" -> w, "h" -> h, "staffY" -> staffY,
      "templateName" -> templateName, "score" -> score)
  }
}
object TemplateMatch {
  def fromMap(map:Map[String,Any]) : TemplateMatch = {
    TemplateMatch(
      map("x").asInstanceOf[Int],
      map("y").asInstanceOf[Int],
      map("w").asInstanceOf[Int],
      map("h").asInstanceOf[Int],
      map("staffY").asInstanceOf[Int],
      map("templateName").asInstanceOf[String],
      map("score").asInstanceOf[Int])
  }
}
