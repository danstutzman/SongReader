import java.lang.Math

object EraseBeams {
  def run(input:GrayImage, beams:List[Beam], staff:Staff, staffName:String) = {
    val output = input.copy
    val multiplier = 2.0f // especially thick because of surrounding gray pixels
    beams.foreach { beam =>
      (beam.x0 to beam.x1).foreach { x =>
        val staffSeparation = staff.staffSeparations(x)
        if (staffSeparation > 0.0f) {
          val expectedBeamWidth =
            Math.round(staffSeparation * multiplier).intValue - 2
          val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
          val yMid = (beam.y0 + (beam.y1 - beam.y0) * progress).intValue
          val yTop = (yMid - expectedBeamWidth/2) max 0
          val yBottom = (yMid + (expectedBeamWidth+1)/2) min (input.h - 1)
          (yTop to yBottom).foreach { y =>
            output(x, y) = 0
          }
        }
      }
    }
    output
  }
}
