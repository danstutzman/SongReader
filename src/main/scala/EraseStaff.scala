import java.io.File
import java.lang.Math
import scala.util.Sorting

object EraseStaff {
  def classifyGoodOrBad(staffs:List[Staff], image:GrayImage, demo:ColorImage) {
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
            }
          }
          if (meanWhiteVs.size > 0) {
            val meanWhite = meanWhiteVs.sum / meanWhiteVs.size.toFloat
            val stdDevWhite = stdDevWhiteVs.sum / stdDevWhiteVs.size.toFloat
            val meanBlack = meanBlackVs.sum / meanBlackVs.size.toFloat
            val stdDevBlack = stdDevBlackVs.sum / stdDevBlackVs.size.toFloat

            val (slope, intercept) =
              FindStaffs.linearRegression(leftPoints ++ rightPoints)
            val predictedY = Math.round(slope * x + intercept).intValue
            (-wavelen5/2 to wavelen5/2).foreach { yNeighbor =>
              val staffYOver2 = yNeighbor * 5.0f / wavelen5
              val closenessToLine =
                Math.abs(staffYOver2 - Math.round(staffYOver2))
              val darkestExpectedV =
                if (closenessToLine > 0.25) meanWhite - stdDevWhite * 3
                else meanBlack - stdDevBlack * 2
              val y = predictedY + yNeighbor
              if (image(x, y) < darkestExpectedV) {
                demo(x, y) = (255, 0, 0)
              }
            }
          } // end if
        } // end if
      } // next x
    } // next staff
  }

  def run(input:GrayImage, staffs:List[Staff], caseName:String) = {
    val demo = input.toColorImage
    classifyGoodOrBad(staffs, input, demo)
    demo.saveTo(new File("demos/goodbad.%s.png".format(caseName)))
/*
    val output = input.copy

    staffs.foreach { staff =>
      val maxStaffSpacing = staff.staffSeparations.max
      val halfStaffSpacing = Math.ceil(maxStaffSpacing / 2).intValue
  
      val yNeighborToMedians = new Array[Array[Int]](halfStaffSpacing)
      (0 until halfStaffSpacing).foreach { i =>
        yNeighborToMedians(i) = new Array[Int](input.w)
      }
  
      (staff.bounds.minX to staff.bounds.maxX).foreach { xOuter =>
        val x0 = (xOuter - 50) max staff.bounds.minX
        val x1 = (xOuter + 50) min staff.bounds.maxX
        val yNeighborToValues = new Array[List[Int]](halfStaffSpacing)
        (0 until halfStaffSpacing).foreach { i =>
          yNeighborToValues(i) = Nil
        }
  
        (-4 to 4 by 2).foreach { staffY =>
          (x0 to x1).foreach { x =>
            val midlineY = staff.midlineYs(x)
            val separation = staff.staffSeparations(x)
            val y = midlineY + Math.round(separation * staffY / 2.0f).intValue
            if (midlineY > -1 && separation > 0) {
              (0 until halfStaffSpacing).foreach { yNeighbor =>
                val v = input(x, y + yNeighbor)
                if (whereNotesAre(x, y + yNeighbor) != 0)
                  yNeighborToValues(yNeighbor) =
                    v ::yNeighborToValues(yNeighbor)
              }
            }
          }
        }
        
        (0 until halfStaffSpacing).foreach { yNeighbor =>
          val values = yNeighborToValues(yNeighbor).toArray
          Sorting.quickSort(values)
          val median = if (values.length > 0) values(values.length / 2) else 0
          yNeighborToMedians(yNeighbor)(xOuter) = median
        }
      }
  
      (staff.bounds.minY to staff.bounds.maxY).foreach { y =>
        (staff.bounds.minX to staff.bounds.maxX).foreach { x =>
          // black means the darkest/middle color in the staff lines
          val black = yNeighborToMedians(0)(x)
          // white means the lightest color; the color outside the staff lines
          val white = yNeighborToMedians(halfStaffSpacing - 1)(x)
  
          // find out the distance to the closest staff line
          var minDistance = 9999
          (-4 to 4 by 2).foreach { staffY =>
            val midlineY = staff.midlineYs(x)
            val separation = staff.staffSeparations(x)
            val yOfStaff =
              midlineY + Math.round(separation * staffY / 2.0f).intValue
            minDistance = minDistance min Math.abs(y - yOfStaff)
          }
          // if it's far from the staff (in ledger-line land), just use
          // the distance corresponding to half way between staff lines
          val distanceFromStaff = minDistance min (halfStaffSpacing - 1)
  
          val v = input(x, y)
  
          // predict the color at this pixel given its location relative
          // to the staff lines
          val expectedV = yNeighborToMedians(distanceFromStaff)(x)
  
          // was pixel darker than the staff line?  Positive diff means darker.
          val diff = expectedV - v
          val normalizedDiff = diff * 80 / ((white - black) max 10)
          val positiveDiff = if (diff > 0) normalizedDiff else 0
  
          // reverse and scale the pixel's color,
          // so its darkest black is full white (255)
          // and its lightest white is full black (0)
          val otherBlack = white / 2
          // the ink at this spot is a little darker than it would be if it
          // weren't overlapping a staff line, so subtract out the expected
          // darkness contribution of the staff line ink
          val multiplier =
            if (v >= otherBlack) 1.0f
            else v.floatValue / otherBlack
          val v2 = v - ((expectedV - white) * multiplier).intValue
          val normalizedV1 =
            255 - ((v2 - otherBlack) * 255 / ((white - otherBlack) max 10))
          val normalizedV2 =
            if (normalizedV1 > 255) 255
            else if (normalizedV1 < 0) 0
            else normalizedV1
  
          output(x, y) = normalizedV2
        }
      }
      //output.saveTo(new File("demos/erase.%s.png".format(staffName)))
  
      // Draw staff lines on image to see if they look right
//      val demo3 = new GrayImage(input.w, input.h)
//      (0 until input.h).foreach { y =>
//        (0 until input.w).foreach { x =>
//          demo3(x, y) = input(x, y)
//        }
//      }
//      (0 until input.w).foreach { x =>
//        val y = staff.midlineYs(x)
//        if (y > -1) {
//          (0 until halfStaffSpacing).foreach { yInner =>
//            demo3(x, y - yInner) = yNeighborToMedians(yInner)(x)
//            demo3(x, y + yInner) = yNeighborToMedians(yInner)(x)
//          }
//        }
//      }
//      demo3.saveTo(new File("demos/erase3.%s.png".format(caseName)))
    }*/

    input //output
  }
}
