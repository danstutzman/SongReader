import java.lang.Math

object EraseBeams {
  def run(input:GrayImage, beams:List[Beam], staffs:List[Staff],
      caseName:String) = {
    val output = input.copy
    val multiplier = 2.0f // especially thick because of surrounding gray pixels
    beams.foreach { beam =>
      var minDistance = 9999
      var closestStaff = staffs(0)
      staffs.foreach { staff =>
        val distance = (Math.abs(staff.bounds.minY - beam.y0)) min
                       (Math.abs(staff.bounds.maxY - beam.y1))
        if (distance < minDistance) {
          minDistance = distance
          closestStaff = staff
        }
      }

      val expectedBeamWidth =
        Math.round(closestStaff.staffSeparations.max * multiplier).intValue - 2
      (beam.x0 to beam.x1).foreach { x =>
        val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
        val yMid = (beam.y0 + (beam.y1 - beam.y0) * progress).intValue
        val yTop = (yMid - expectedBeamWidth/2) max 0
        val yBottom = (yMid + (expectedBeamWidth+1)/2) min (input.h - 1)
        (yTop to yBottom).foreach { y =>
          output(x, y) = 0
        }
      }
    }
    output
  }
}
