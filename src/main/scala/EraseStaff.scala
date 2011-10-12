import java.io.File
import java.lang.Math
import scala.util.Sorting

object EraseStaff {
  def separateNotes(input:GrayImage, staffName:String) = {
    val ceilings = new Array[Int](input.w)
    (0 until input.w).foreach { x =>
      val x0 = (x - 50) max 0
      val x1 = (x + 50) min (input.w - 1)
      val values = new Array[Int]((x1 - x0 + 1) * input.h)
      var i = 0
      (x0 to x1).foreach { xNeighbor =>
        (0 until input.h).foreach { y =>
          values(i) = input(xNeighbor, y)
          i += 1
        }
      }
      Sorting.quickSort(values)
      ceilings(x) = values(values.length * 1/4)
    }

    val floors = new Array[Int](input.w)
    (0 until input.w).foreach { x =>
      floors(x) = ceilings(x) * 1/2
    }

    val adjusted = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = input(x, y)
        val newV = (v - floors(x)) * 255 / (ceilings(x) - floors(x) + 1)
        adjusted(x, y) = if (newV < 0) 0 else if (newV > 255) 255 else newV
      }
    }
    //adjusted.saveTo(new File("demos/adjusted.%s.png".format(staffName)))

    val augmentedBinaryNonStaff = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val v = input(x, y)
        val newV = (v - floors(x)) * 255 / (ceilings(x) - floors(x) + 1)
        val x0 = (x - 1) max 0
        val x1 = (x + 1) min (input.w - 1)
        val y0 = (y - 1) max 0
        val y1 = (y + 1) min (input.h - 1)
 
        val isCloseToFloor = (x0 to x1).exists { xNeighbor =>
          (y0 to y1).exists { yNeighbor =>
            input(xNeighbor, yNeighbor) <= floors(x)
          }
        }

        augmentedBinaryNonStaff(x, y) = if (isCloseToFloor) 0 else 255
      }
    }
    
    val partiallyErased = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        partiallyErased(x, y) =
          if (augmentedBinaryNonStaff(x, y) == 0) 255
          else adjusted(x, y)
      }
    }
    //partiallyErased.saveTo(new File(
    //  "demos/partially_erased.%s.png".format(staffName)))

    (adjusted, partiallyErased, augmentedBinaryNonStaff)
  }

  def run(input:GrayImage, staffs:List[Staff], caseName:String) = {
    val output = input.copy
    val (_, _, whereNotesAre) = separateNotes(input, caseName)

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
      /*val demo3 = new GrayImage(input.w, input.h)
      (0 until input.h).foreach { y =>
        (0 until input.w).foreach { x =>
          demo3(x, y) = input(x, y)
        }
      }
      (0 until input.w).foreach { x =>
        val y = staff.midlineYs(x)
        if (y > -1) {
          (0 until halfStaffSpacing).foreach { yInner =>
            demo3(x, y - yInner) = yNeighborToMedians(yInner)(x)
            demo3(x, y + yInner) = yNeighborToMedians(yInner)(x)
          }
        }
      }
      demo3.saveTo(new File("demos/erase3.%s.png".format(staffName)))*/
    }

    output
  }
}
