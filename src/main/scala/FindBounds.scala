import java.io.File
import java.lang.Math
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.util.Random

object FindBounds {
  case class Segment(val y0:Int, val y1:Int) {}

  def run(highlighted:ColorImage, caseName:String) = {
    val yToStrength = new Array[Int](highlighted.h + 1)
    (0 until highlighted.h).foreach { y =>
      var strength = 0
      (0 until highlighted.w).foreach { x =>
        if (highlighted(x, y)._1 > 0) {
          strength += 1
        }
      }
      yToStrength(y) = strength
    }

    val strengthsSorted = yToStrength.clone
    Sorting.quickSort(strengthsSorted)

    val beginThreshold = strengthsSorted(highlighted.h * 7/8)
    val endThreshold = strengthsSorted(highlighted.h * 6/8)
    var currentY0 = 0
    val isInSegment = new Array[Boolean](highlighted.h)
    var wasInSegment = false
    var segmentClock = 0
    (0 to highlighted.h).foreach { y =>
      if (wasInSegment) {
        if (yToStrength(y) < endThreshold) {
          if (segmentClock == 0) {
            (currentY0 until y).foreach { yPast =>
              isInSegment(yPast) = true
            }
            wasInSegment = false
          } else {
            segmentClock -= 1
          }
        }
      } else {
        val inSegment = (yToStrength(y) >= beginThreshold)
        if (inSegment) {
          currentY0 = y
          wasInSegment = true
          segmentClock = 10
        }
      }
    }

    // go backwards
    currentY0 = highlighted.h
    (highlighted.h to 0 by - 1).foreach { y =>
      if (wasInSegment) {
        if (yToStrength(y) < endThreshold) {
          if (segmentClock == 0) {
            (currentY0 until y by -1).foreach { yPast =>
              isInSegment(yPast) = true
            }
            wasInSegment = false
          } else {
            segmentClock -= 1
          }
        }
      } else {
        val inSegment = (yToStrength(y) >= beginThreshold)
        if (inSegment) {
          currentY0 = y
          wasInSegment = true
          segmentClock = 10
        }
      }
    }

    var segments = List[Segment]()
    currentY0 = 0
    (1 until highlighted.h).foreach { y =>
      if (isInSegment(y - 1) && !isInSegment(y)) {
        segments = Segment(currentY0, y - 1) :: segments
      }
      else if (!isInSegment(y - 1) && isInSegment(y)) {
        currentY0 = y
      }
    }

    val demo = highlighted.copy
    segments.foreach { segment =>
      (segment.y0 to segment.y1).foreach { y =>
        demo(0, y) = (0, 255, 0)
      }
    }
    demo.saveTo(new File("demos/split.%s.png".format(caseName)))

    segments.map { segment =>
      BoundingBox(0, highlighted.w - 1, segment.y0, segment.y1)
    }
  }
}
