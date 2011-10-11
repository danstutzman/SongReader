case class Staff (
  val staffName:String,
  val bounds:BoundingBox,
  val midlineYs:Array[Int],
  val staffSeparations:Array[Float]
) {
  def toMap() : Map[String,Any] = {
    Map("staffName"        -> staffName,
        "bounds"           -> bounds.toMap,
        "midlineYs"        -> midlineYs,
        "staffSeparations" -> staffSeparations)
  }
}
object Staff {
  def fromMap(map:Map[String,Any]) : Staff = {
    val staffName = map("staffName").asInstanceOf[String]
    val bounds =
      BoundingBox.fromMap(map("bounds").asInstanceOf[Map[String,Int]])
    val midlineYs = map("midlineYs").asInstanceOf[List[Int]].toArray
    val staffSeparations = map("staffSeparations"
      ).asInstanceOf[List[BigDecimal]].map { _.toFloat }.toArray
    Staff(staffName, bounds, midlineYs, staffSeparations)
  }
}
