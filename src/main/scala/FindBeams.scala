import java.io.File
import java.lang.Math

object FindBeams {
  def findThickHorizontalLines(
      justNotes:GrayImage, staffs:List[Staff], caseName:String) = {
    
    val topEdges = ImageFilter.edgeDetection(
      justNotes, Array(-1, -2, -1, 0, 0, 0, 1, 2, 1, 4))
    val topEdgesBlurred = ImageFilter.edgeDetection(
      topEdges, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(30)
    val bottomEdges = ImageFilter.edgeDetection(
      justNotes, Array(1, 2, 1, 0, 0, 0, -1, -2, -1, 4))
    val bottomEdgesBlurred = ImageFilter.edgeDetection(
      bottomEdges, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(30)
    //val blurred = edgeDetection(
    //  justNotes.inverse, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(200)

    val thickLines = new ColorImage(justNotes.w, justNotes.h)
    val multiplier = 0.8f
    (0 until justNotes.h).foreach { y =>
      var minDistance = 9999
      var closestStaff:Option[Staff] = None
      staffs.foreach { staff =>
        val distance = (Math.abs(staff.bounds.minY - y)) min
                       (Math.abs(staff.bounds.maxY - y))
        if (distance < minDistance) {
          minDistance = distance
          closestStaff = Some(staff)
        }
      }

      closestStaff.foreach { closestStaff =>
        val expectedBeamWidth = Math.round(closestStaff.staffSeparations.max *
          multiplier).intValue - 2
        (0 until justNotes.w).foreach { x =>
          val r = topEdgesBlurred(x, y - expectedBeamWidth / 2)
          val g = bottomEdgesBlurred(x, y + (expectedBeamWidth + 1) / 2)
          //val b = blurred(x, y)
          thickLines(x, y) = (r, g, 0)
        }
      }
    }
    thickLines.saveTo(new File("demos/beamlike.%s.png".format(caseName)))
    thickLines
  }

  def findBeams(thickLines:ColorImage, image:GrayImage, caseName:String) = {
    val minBeamLength = 25
    val threshold = 23
    val demo = image.toColorImage
    var beams:List[Beam] = Nil
    (-4 to 4).foreach { slopeTenths =>
      val y0 = (0 - (image.w * slopeTenths / 10)) min 0
      val y1 = (image.h + (image.w * slopeTenths / 10)) max (image.h - 1)
      (y0 to y1).foreach { startingY =>
        var numBlacksInWindow = 0
        (0 until image.w).foreach { x =>
          val y = startingY + (x * slopeTenths / 10)
          if (thickLines(x, y) == (255, 255, 0))
            numBlacksInWindow += 1
    
          val windowX = x - minBeamLength
          val windowY = startingY + (windowX * slopeTenths / 10)
          if (thickLines(windowX, windowY) == (255, 255, 0))
            numBlacksInWindow -= 1
    
          if (numBlacksInWindow >= threshold) {
            var foundBeam = false
            val newX0 = x - threshold
            val newX1 = x - 1
            val newY0 = startingY + (newX0 * slopeTenths / 10)
            val newY1 = y
            beams.foreach { box =>
              if (!foundBeam && newX0 <= box.x1 + 5 && newX1 >= box.x0 - 5) {
                if (newY0 <= box.y1 + 2 && newY1 >= box.y0 - 2) {
                  val newBeam = Beam(box.x0 min newX0, box.x1 max newX1,
                                   box.y0 min newY0, box.y1 max newY1)
                  beams = newBeam :: beams.filter { _ != box }
                  foundBeam = true
                }
                else if (newY1 <= box.y0 + 2 && newY0 >= box.y1 - 2) {
                  val newBeam = Beam(box.x0 min newX0, box.x1 max newX1,
                                   box.y0 max newY0, box.y1 min newY1)
                  beams = newBeam :: beams.filter { _ != box }
                  foundBeam = true
                }
              }
            }
            if (!foundBeam) {
              val newBeam = Beam(newX0, newX1, newY0, newY1)
              beams = newBeam :: beams
            }
    
            (windowX to x).foreach { xBefore =>
              demo(xBefore, y - (x - xBefore) * slopeTenths / 10) = (255, 0, 0)
            }
          }
        }
      }
    }
    demo.saveTo(new File("demos/beams.%s.png".format(caseName)))

    val beamsCropped = beams.map { beam =>
      Beam(beam.x0 max 0, beam.x1 min (image.w - 1),
           beam.y0 max 0, beam.y1 min (image.h - 1))
    }
    beamsCropped
  }

  def demoBeams(beams:List[Beam], image:GrayImage, caseName:String) {
    val demo2 = image.toColorImage
    val red = (255, 0, 0)
    beams.foreach { beam =>
      (beam.x0 to beam.x1).foreach { x =>
        val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
        val y = beam.y0 + ((beam.y1 - beam.y0) * progress).intValue
        demo2(x, y) = red
      }
    }
    demo2.saveTo(new File("demos/beamboxes.%s.png".format(caseName)))
  }

  def run(justNotes:GrayImage, image:GrayImage, staffs:List[Staff],
      caseName:String) = {
    val thickLines = findThickHorizontalLines(justNotes, staffs, caseName)
    val beams = findBeams(thickLines, image, caseName)
    demoBeams(beams, image, caseName)
    beams
  }
}
