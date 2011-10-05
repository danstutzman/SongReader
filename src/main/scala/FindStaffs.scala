import java.io.File
import java.lang.Math
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.util.Random

object FindStaffs {
  case class Path(val yForI:List[Float], val strength:Int) {}

  def determineBestSlopes(segment:BoundingBox, highlighted:ColorImage,
      demo:ColorImage, x0:Int, x1:Int, caseName:String) = {
    val midX = (x0 + x1) / 2

    var maxStrength = 0
    var argmaxYCenterIntercept = 0
    var argmaxSlope = 0.0f
    (-10 to 10).foreach { slopeCents =>
      val slope = slopeCents / 20.0f
      (segment.minY to segment.maxY).foreach { yCenterIntercept =>
        var strength = 0
        (x0 until x1).foreach { x =>
          val y = yCenterIntercept +
            Math.round((x - midX) * slope).intValue
          strength += highlighted(x, y)._1
        }
  
        if (strength > maxStrength) {
          maxStrength = strength
          argmaxSlope = slope
          argmaxYCenterIntercept = yCenterIntercept
        }
      }
    }

    // now split the line half way
    var strength2 = 0
    var halfWayX = 0
    (x0 to x1).foreach { x =>
      val y = argmaxYCenterIntercept +
        Math.round((x - midX) * argmaxSlope).intValue
      strength2 += highlighted(x, y)._1
      if (strength2 >= maxStrength / 2 && halfWayX == 0) {
        halfWayX = x
      }
    }

    // consider other intercepts for the same slope
    var allYCenterIntercepts = List[Int]()
    (segment.minY to segment.maxY).foreach { yCenterIntercept =>
      var strength = 0
      (x0 until x1).foreach { x =>
        val y = yCenterIntercept +
          Math.round((x - midX) * argmaxSlope).intValue
        strength += highlighted(x, y)._1
      }
 
      if (strength > maxStrength / 2) {
        allYCenterIntercepts = yCenterIntercept :: allYCenterIntercepts
      }
    }

    // if intercepts are adjacent, just keep the center one
    // for example, [11, 12, 34, 35, 36, 99] changes to [11, 35, 99]
    var newIntercepts = List[Int]()
    var lastY = -1
    var startOfRun = -1
    var inRun = false
    var wasInRun = false
    allYCenterIntercepts.reverse.foreach { y =>
      val inRun = (y == lastY + 1)

      if (!wasInRun && inRun) {
        startOfRun = lastY
        wasInRun = true
      }
      else if (wasInRun && !inRun) {
        newIntercepts = (startOfRun + lastY) / 2 :: newIntercepts
        wasInRun = false
      }
      else if (!wasInRun && !inRun && lastY > -1) {
        newIntercepts = lastY :: newIntercepts
      }

      lastY = y
      wasInRun = inRun
    }
    if (wasInRun) {
      newIntercepts = (startOfRun + lastY) / 2 :: newIntercepts
    } else {
      newIntercepts = lastY :: newIntercepts
    }
    allYCenterIntercepts = newIntercepts

    allYCenterIntercepts.foreach { yCenterIntercept =>
      (x0 to x1).foreach { x =>
        val y = yCenterIntercept +
          Math.round((x - midX) * argmaxSlope).intValue
        if (x > halfWayX - 5 && x < halfWayX + 5) {
          demo(x, y) = (255, 255, 255)
        }
      }
    }
    val halfWayYs = allYCenterIntercepts.map { yCenterIntercept =>
      yCenterIntercept + (halfWayX - midX) * argmaxSlope
    }

    (halfWayX, argmaxSlope, halfWayYs)
  }

  def findMidlineYs(segment:BoundingBox,
      highlighted:ColorImage, demo:ColorImage, caseName:String) :
      (Array[Int],Array[Float]) = {
    var halfWayXsList = List[Int]()
    var slopesList = List[Float]()
    var halfWayYsList = List[List[Float]]()
    (0 to (highlighted.w / 100 * 100 - 50) by 50).foreach { x0 =>
      val (halfWayX, slope, halfWayYs) = determineBestSlopes(
        segment, highlighted, demo, x0, x0 + 100 - 1, caseName)
      halfWayXsList = halfWayX :: halfWayXsList
      slopesList = slope :: slopesList
      halfWayYsList = halfWayYs :: halfWayYsList
    }

    val halfWayXs = halfWayXsList.reverse.toArray
    val slopes = slopesList.reverse.toArray
    val halfWayYs = halfWayYsList.reverse.toArray
    var paths = halfWayYs(0).map { y => Path(List(y), 0) }
    (0 until slopes.size - 1).foreach { i =>
      val fromSlope = slopes(i)
      val toSlope = slopes(i + 1)
      val fromX = halfWayXs(i)
      val toX = halfWayXs(i + 1)
      paths = paths.map { path =>
        val fromY = path.yForI.head
        var maxStrength = (toX - fromX) * 255 / 4 // threshold for default
        var argmaxToY = -1.0f // default (no line)
        halfWayYs(i + 1).foreach { toY =>
          var strength = 0
          (fromX to toX).foreach { x =>
            val progress = (x - fromX) / (toX - fromX).floatValue
            val y = Math.round(fromY + (toY - fromY) * progress).intValue
            strength += highlighted(x, y)._1
            //demo(x, y) = (0, 255, 0)
          }
          if (strength > maxStrength) {
            maxStrength = strength
            argmaxToY = toY
          }
        }

        //(fromX to toX).foreach { x =>
        //  val progress = (x - fromX) / (toX - fromX).floatValue
        //  val y = Math.round(fromY + (argmaxToY - fromY) * progress).intValue
        //  demo(x, y) = (0, 255, 0)
        //}

        Path(argmaxToY :: path.yForI, path.strength + maxStrength)
      }
    }

    // Choose best path
    var maxStrength = 0
    var argmaxYForI = Array[Float]()
    paths.foreach { path =>
      if (path.strength > maxStrength) {
        maxStrength = path.strength
        argmaxYForI = path.yForI.reverse.toArray
      }
    }

    val midlineYs = new Array[Int](highlighted.w)
    val staffSeparations = new Array[Int](highlighted.w)
    (0 until midlineYs.size).foreach { midlineYs(_) = -1 }
    (0 until staffSeparations.size).foreach { staffSeparations(_) = -1 }

    if (argmaxYForI(0) != -1 && argmaxYForI(1) != -1) {
      (0 until halfWayXs(0)).foreach { x =>
        val progress =
          (x - halfWayXs(0)) / (halfWayXs(1) - halfWayXs(0)).floatValue
        val y = Math.round(argmaxYForI(0) +
          (argmaxYForI(1) - argmaxYForI(0)) * progress).intValue
        midlineYs(x) = y
        staffSeparations(x) = highlighted(x, y)._2 // stored in green channel
        demo(x, y) = (0, 255, 0)
      }
    }

    (0 until argmaxYForI.size - 1).foreach { i =>
      val fromX = halfWayXs(i)
      val toX = halfWayXs(i + 1)
      val fromY = argmaxYForI(i)
      val toY = argmaxYForI(i + 1)
      if (fromY != -1 && toY != -1) {
        (fromX to toX).foreach { x =>
          val progress = (x - fromX) / (toX - fromX).floatValue
          val y = Math.round(fromY + (toY - fromY) * progress).intValue
          midlineYs(x) = y
          staffSeparations(x) = highlighted(x, y)._2 // stored in green channel
          demo(x, y) = (0, 255, 0)
        }
      }
    }

    // Smooth staffSeparations
    var smoothedStaffSeparations = new Array[Float](highlighted.w)
    (0 until staffSeparations.size).foreach { x =>
      val x0 = (x - 20) max 0
      val x1 = (x + 20) min (staffSeparations.size - 1)
      var sum = 0
      var denom = 0
      (x0 to x1).foreach { x =>
        if (staffSeparations(x) != -1 && staffSeparations(x) > 0) {
          sum += staffSeparations(x)
          denom += 1
        }
      }
      smoothedStaffSeparations(x) = sum / (denom max 1).floatValue
    }

    (midlineYs, smoothedStaffSeparations)
  }

  def run(highlighted:ColorImage, image:GrayImage, segments:List[BoundingBox],
      caseName:String) : List[Staff] = {
    val demo = highlighted.copy
    val demo2 = image.toColorImage
    val red = (255, 0, 0)

    var staffs = segments.map { segment =>
      val (midlineYs, staffSeparations) =
        findMidlineYs(segment, highlighted, demo, caseName)

      (0 until highlighted.w).foreach { x =>
        val midlineY = midlineYs(x)
        val staffSeparation = staffSeparations(x)
        if (midlineY > -1 && staffSeparation > 0) {
          demo2(x, midlineY + Math.round(staffSeparation * -2).intValue) = red
          demo2(x, midlineY + Math.round(staffSeparation * -1).intValue) = red
          demo2(x, midlineY + Math.round(staffSeparation *  0).intValue) = red
          demo2(x, midlineY + Math.round(staffSeparation *  1).intValue) = red
          demo2(x, midlineY + Math.round(staffSeparation *  2).intValue) = red
        }
      }

      Staff(midlineYs, staffSeparations)
    }

    demo.saveTo(new File("demos/hline.%s.png".format(caseName)))
    demo2.saveTo(new File("demos/newstaff.%s.png".format(caseName)))

    staffs
  }
}
