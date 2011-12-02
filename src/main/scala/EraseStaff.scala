import java.io.File
import java.lang.Math
import scala.util.Sorting

object EraseStaff {
  def run(image:GrayImage, staffs:List[Staff], caseName:String) = {
    val howClose = 20 // number of neighbors to the left and right to consider
    def getNeighborPoints(xToBestY:Array[Int], x:Int) = {
      val leftPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeLeftPoints = leftPoints.slice(0, x
        ).filter { _._2 != 0 }.takeRight(howClose).toList
      val rightPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeRightPoints = rightPoints.slice(x + 1, xToBestY.size
        ).filter { _._2 != 0 }.take(howClose).toList
      (closeLeftPoints, closeRightPoints)
    }

    val prob = new GrayImage(image.w, image.h).inverse // all white image
    val threshold = -20
    staffs.foreach { staff =>
      val xToY = staff.midlineYs
      (0 until xToY.size).foreach { x =>
        val centerY = xToY(x)
        val wavelen5 = Math.round(staff.staffSeparations(x) * 5.0f).intValue
        val (leftPoints, rightPoints) = getNeighborPoints(xToY, x)
        if (leftPoints.size > 0 && rightPoints.size > 0) {
          var meanWhiteVs = List[Float]()
          var stdDevWhiteVs = List[Float]()
          var meanBlackVs = List[Float]()
          var stdDevBlackVs = List[Float]()
          var positionToVs:Array[List[Float]] =
            (0 to wavelen5/2).map { i => List[Float]() }.toArray
          (leftPoints ++ rightPoints).foreach { xy =>
            var blackVs = List[Int]()
            var whiteVs = List[Int]()
            val (x2, centerY2) = xy
            List(-4, -2, 0, 2, 4).foreach { staffY =>
              val y2 = centerY2 + Math.round(wavelen5 * staffY / 10).intValue
              var minV = 999999
              ((y2 - 2 max 0) to (y2 + 2 min image.h - 1)).foreach { y3 =>
                val v = image(x2, y3)
                if (v < minV)
                  minV = v
              }
              blackVs = minV :: blackVs
            }
            List(-5, -3, -1, 1, 3, 5).foreach { staffY =>
              val y2 = centerY2 + Math.round(wavelen5 * staffY / 10).intValue
              whiteVs = image(x2, y2) :: whiteVs
            }
            val meanWhite = whiteVs.sum / whiteVs.size.toFloat
            val varianceWhite = whiteVs.map { v =>
              Math.pow(meanWhite - v, 2) }.sum / whiteVs.size.toFloat
            val stdDevWhite = Math.sqrt(varianceWhite).toFloat
            val meanBlack = blackVs.sum / blackVs.size.toFloat
            val varianceBlack = blackVs.map { v =>
              Math.pow(meanBlack - v, 2) }.sum / blackVs.size.toFloat
            val stdDevBlack = Math.sqrt(varianceBlack).toFloat
            if (stdDevWhite < 10) {
              meanWhiteVs = meanWhite :: meanWhiteVs
              stdDevWhiteVs = stdDevWhite :: stdDevWhiteVs
              meanBlackVs = meanBlack :: meanBlackVs
              stdDevBlackVs = stdDevBlack :: stdDevBlackVs
              (-wavelen5/2 to wavelen5/2).foreach { offsetY =>
                val y2 = centerY2 + offsetY
                positionToVs(Math.abs(offsetY)) = image(x2, y2) ::
                  positionToVs(Math.abs(offsetY))
              }
            }
          }
          if (meanWhiteVs.size > 0) {
            val meanWhite = meanWhiteVs.sum / meanWhiteVs.size.toFloat
            val stdDevWhite = stdDevWhiteVs.sum / stdDevWhiteVs.size.toFloat
            val meanBlack = meanBlackVs.sum / meanBlackVs.size.toFloat
            val stdDevBlack = stdDevBlackVs.sum / stdDevBlackVs.size.toFloat

            // this covers the range outside the staff, so ledger lines
            // aren't missed.
            (-wavelen5*2/2 to wavelen5*2/2).foreach { offsetY =>
              val vs =
                if (Math.abs(offsetY) < positionToVs.size)
                  positionToVs(Math.abs(offsetY))
                else
                  positionToVs(positionToVs.size - 1)
              val meanV = vs.sum / vs.size
              val varianceV =
                vs.map { v => Math.pow(meanV - v, 2) }.sum / vs.size
              val stdDevV = Math.sqrt(varianceV)
              val y = centerY + offsetY
              if (y >= 0 && y < image.h) {
                val v = ((image(x, y) - meanV) / stdDevV * 32).intValue
                if (v <= -255 + threshold)
                  prob(x, y) = 0
                else if (v <= threshold)
                  prob(x, y) = 255 + v - threshold
                else
                  prob(x, y) = 255
              }
//              if (image(x, y) < (meanV - stdDevV * 2).intValue) {
//                demo(x, centerY + offsetY) = (255, 0, 0)
//              }
/*              val staffYOver2 = yNeighbor * 5.0f / wavelen5
              val closenessToLine =
                Math.abs(staffYOver2 - Math.round(staffYOver2))
              val darkestExpectedV =
                if (closenessToLine > 0.25) meanWhite - stdDevWhite * 3
                else meanBlack - stdDevBlack * 2
              val y = predictedY + yNeighbor
              if (image(x, y) < darkestExpectedV) {
                demo(x, y) = (255, 0, 0)
              }*/
            }
          } // end if
        } // end if
      } // next x
    } // next staff
    prob.saveTo(new File("demos/prob.%s.png".format(caseName)))
    prob.inverse // oops, notes should be white on black not black on white
  }
}
