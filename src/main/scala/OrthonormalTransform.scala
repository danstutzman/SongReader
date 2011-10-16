case class OrthonormalTransform (
  val staff:Staff,
  val staffSeparationsMax:Float, // just here for speed
  val m0:Float, // v slope at left of image
  val m1:Float, // v slope at right of image
  val w:Int, // width of image
  val bounds:BoundingBox, // subtract minX and minY to find real coordinates
  val yForStaffY:Map[Int,Int],
  val xForXIntercept:Array[Int]
) {
  def transformXY(x:Int, y:Int) = {
    (Ocr4Music.targetXFor(x, y, m0, m1, w) - bounds.minX,
     Ocr4Music.targetYFor(x, y, staff, staffSeparationsMax) - bounds.minY)
  }
  def toMap() : Map[String,Any] = {
    Map("staff" -> staff.toMap,
        "staffSeparationsMax" -> staffSeparationsMax,
        "m0" -> m0, "m1" -> m1, "w" -> w,
        "bounds"         -> bounds.toMap,
        "yForStaffY"     -> yForStaffY.map { pair => List(pair._1, pair._2) },
        "xForXIntercept" -> xForXIntercept)
  }
}
object OrthonormalTransform {
  def fromMap(map:Map[String,Any]) : OrthonormalTransform = {
    val staff = Staff.fromMap(map("staff").asInstanceOf[Map[String,Any]])
    val staffSeparationsMax =
      map("staffSeparationsMax").asInstanceOf[BigDecimal].toFloat
    val m0 = map("m0").asInstanceOf[BigDecimal].toFloat
    val m1 = map("m1").asInstanceOf[BigDecimal].toFloat
    val w = map("w").asInstanceOf[Int]
    val bounds =
      BoundingBox.fromMap(map("bounds").asInstanceOf[Map[String,Int]])
    val yForStaffY = map("yForStaffY").asInstanceOf[List[List[Int]]].flatMap {
      pair => List((pair(0), pair(1)))
    }.toMap
    val xForXIntercept = map("xForXIntercept").asInstanceOf[List[Int]].toArray
    OrthonormalTransform(staff, staffSeparationsMax, m0, m1, w, bounds,
      yForStaffY, xForXIntercept)
  }
}
