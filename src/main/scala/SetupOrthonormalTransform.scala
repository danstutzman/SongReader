import java.lang.Math

object SetupOrthonormalTransform {
  // by "orthonormal" I mean that the vertical lines in the image are pointing
  // straight up and down, and the horizontal staff lines are pointing
  // straight left and right, instead of both being somewhat diagonal.
  // It doesn't mean that the x and y vectors are unit length.
  def run(w:Int, h:Int, staff:Staff, vLineSlopeRange:(Float,Float),
      staffName:String) : OrthonormalTransform ={
    val (m0, m1) = vLineSlopeRange
    val staffSeparationsMax = staff.staffSeparations.max // calc only once

    val minTargetY = (0 until w).map {
      Ocr4Music.targetYFor(_, 0, staff, staffSeparationsMax)
    }.min
    val maxTargetY = (0 until w).map {
      Ocr4Music.targetYFor(_, h - 1, staff, staffSeparationsMax)
    }.max

    val minTargetX = Ocr4Music.targetXFor(0, 0, m0, m1, w) min
                     Ocr4Music.targetXFor(0, h - 1, m0, m1, w)
    val maxTargetX = Ocr4Music.targetXFor(w - 1, 0, m0, m1, w) max
                     Ocr4Music.targetXFor(w - 1, h - 1, m0, m1, w)

    def yForStaffY(staffY:Int) = {
      val y = Math.round(staffY/2.0f * staffSeparationsMax).intValue
      y - minTargetY
    }

    val xForXIntercept = (0 until w).map {
      Ocr4Music.targetXFor(_, 0, m0, m1, w) - minTargetX
    }.toArray
    val yForStaffYMap = (-8 to 8).foldLeft(Map[Int,Int]()) { (map, staffY) =>
      map.updated(staffY, yForStaffY(staffY))
    }
    val transformXY = { (x:Int, y:Int) =>
      (Ocr4Music.targetXFor(x, y, m0, m1, w) - minTargetX,
       Ocr4Music.targetYFor(x, y, staff, staffSeparationsMax) - minTargetY)
    }

    val bounds = BoundingBox(minTargetX, maxTargetX, minTargetY, maxTargetY)
    OrthonormalTransform(
      staff, staffSeparationsMax, m0, m1, bounds, yForStaffYMap, xForXIntercept)
  }
}
