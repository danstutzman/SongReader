case class BoxOfTemplates (
  val box:BoundingBox,
  val templates:Set[TemplateMatch],
  val childBoxes:List[BoxOfTemplates]
) {
  def toMap() : Map[String,Any] = {
    Map("box"       -> box.toMap,
        "templates" -> templates.map { _.toMap }.toList,
        "childBoxes" -> childBoxes.map { _.toMap })
  }
}
object BoxOfTemplates {
  def fromMap(map:Map[String,Any]) : BoxOfTemplates = {
    val box = BoundingBox.fromMap(map("box").asInstanceOf[Map[String,Int]])
    val templates = map("templates").asInstanceOf[List[Map[String,Any]]].map {
      map2 => TemplateMatch.fromMap(map2.asInstanceOf[Map[String,Any]])
    }.toSet
    val childBoxes = map("childBoxes").asInstanceOf[List[Map[String,Any]]].map {
      map2 => BoxOfTemplates.fromMap(map2.asInstanceOf[Map[String,Any]])
    }
    BoxOfTemplates(box, templates, childBoxes)
  }
}
