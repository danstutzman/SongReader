import java.io.File
import java.lang.Math
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.util.Random

case class Segment(val y0:Int, val y1:Int) {}

case class Path(val yForI:List[Float], val strength:Int) {}

object FullPage {
  def expandCaseNames(args:Array[String]) = {
    var caseNames:List[String] = Nil
    val filenames = new File("input").listFiles
    args.foreach { arg =>
      val GlobMatch = ("(" + arg.replaceAll("\\*", ".*") + ")\\.json").r
      filenames.foreach { filename =>
        filename.getName match {
          case GlobMatch(caseName) => caseNames = caseName :: caseNames
          case _ => ()
        }
      }
    }
    if (args.length == 0)
      throw new RuntimeException("1st arg: case name from input/*.json")
    caseNames.reverse
  }

  def highlightStaffLines(image:GrayImage, caseName:String) = {
    val demo = new ColorImage(image.w, image.h)
    
    (0 until image.w).foreach { x =>
      val column = new Array[Int](image.h)
      (0 until image.h).foreach { y =>
        column(y) = image(x, y)
      }

      (6 to 14).foreach { staffSeparation =>
        val y0 = staffSeparation * 2
        val y1 = image.h - 1 - (staffSeparation * 2)
        (y0 to y1).foreach { y =>
          val v = column(y)
  
          var blackValues = new ArrayBuffer[Int]()
          var whiteValues = new ArrayBuffer[Int]()
          (-staffSeparation*2 to staffSeparation*2).foreach { yNeighbor =>
            val v = column(y + yNeighbor)
            val remainder = ((yNeighbor + staffSeparation*2) % staffSeparation)
            if (remainder == 0)
              blackValues.append(v)
            else if (remainder > 2 && remainder < staffSeparation - 1)
              whiteValues.append(v)
          }
  
          val sortedBlackValues = blackValues.toArray
          Sorting.quickSort(sortedBlackValues)

          val sortedWhiteValues = whiteValues.toArray
          Sorting.quickSort(sortedWhiteValues)

          val lightestBlack = sortedBlackValues(sortedBlackValues.size - 1)
          val midWhite = sortedWhiteValues(sortedWhiteValues.size / 2)
          val r = (if (midWhite - lightestBlack > 5) 255 else 0)
  
          val (rOld, gOld, bOld) = demo(x, y)
          if (r > rOld)
            demo(x, y) = (r, staffSeparation, image(x, y))
        }
      }
    }
    demo.saveTo(new File("demos/find12.%s.png".format(caseName)))
    demo
  }

/*
  class Track(
    var lastX:Int,
    var lastY0:Int,
    var lastY1:Int,
    var yForX:Array[Int],
    var strength:Int
  ) {}

  def trackHighlights(highlighted:ColorImage, caseName:String) = {
    val demo = new ColorImage(highlighted.w, highlighted.h)
    var tracks = List[Track]()
    (0 until highlighted.w).foreach { x =>
      (0 until highlighted.h).foreach { y =>
        var (r, _, _) = highlighted(x, y)
        if (r > 40) {
          var existingTrack:Option[Track] = None
          tracks.foreach { track =>
            if (Math.abs(track.lastX - x) <= 30 &&
               (Math.abs(track.lastY0 - y) <= 3 ||
                Math.abs(track.lastY1 - y) <= 3)) {
              existingTrack = existingTrack match {
                case None =>
                  Some(track)
                case Some(otherTrack) =>
                  if (track.strength > otherTrack.strength) Some(track)
                  else existingTrack
              }
            }
          }

          existingTrack match {
            case None =>
              val yForX = new Array[Int](highlighted.w)
              tracks = new Track(x, y, y, yForX, 0) :: tracks
              demo(x, y - 2) = (255, 0, 0)
              demo(x, y - 1) = (255, 0, 0)
              demo(x, y) = (255, 0, 0)
            case Some(track) =>
              if (track.lastX == x) {
                track.lastY1 = y
              } else {
                track.yForX(track.lastX) = (track.lastY0 + track.lastY1) / 2
                track.lastX = x
                track.lastY0 = y
                track.lastY1 = y
              }
              track.strength += 1
              demo(x, y) = (127, 127, 127)
          }
        }
      }
    }

    val demo2 = new ColorImage(highlighted.w, highlighted.h)
    val random = new Random(0)
    tracks.filter { _.strength > 200 }.foreach { track =>
      val color = (random.nextInt(128) + 64,
                   random.nextInt(256),
                   random.nextInt(256))
      (0 until highlighted.w).foreach { x =>
        val y = track.yForX(x)
        if (y > 0) {
          demo2(x, y) = color
        }
      }
    }

    demo2.saveTo(new File("demos/track.%s.png".format(caseName)))
    demo2
  }
*/

  def divideIntoSegments(highlighted:ColorImage, caseName:String) = {
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

    segments
  }

  def determineBestSlopes(segment:Segment, highlighted:ColorImage,
      demo:ColorImage, x0:Int, x1:Int, caseName:String) = {
    val midX = (x0 + x1) / 2

    var maxStrength = 0
    var argmaxYCenterIntercept = 0
    var argmaxSlope = 0.0f
    (-10 to 10).foreach { slopeCents =>
      val slope = slopeCents / 20.0f
      (segment.y0 to segment.y1).foreach { yCenterIntercept =>
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
    (segment.y0 to segment.y1).foreach { yCenterIntercept =>
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

  def findMidlineYs(segment:Segment, highlighted:ColorImage, demo:ColorImage,
      caseName:String) = {
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

  def processCase(caseName:String) {
    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage
    val highlighted = highlightStaffLines(image, caseName)
    //val highlighted = ColorImage.readFromFile(new File(
    //  "demos/find12.%s.png".format(caseName)))
    val segments = divideIntoSegments(highlighted, caseName)

    val demo = highlighted.copy
    val demo2 = image.toColorImage
    val red = (255, 0, 0)
    segments.foreach { segment =>
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
    }
    demo.saveTo(new File("demos/hline.%s.png".format(caseName)))
    demo2.saveTo(new File("demos/newstaff.%s.png".format(caseName)))
  }

  def main(args:Array[String]) {
    val caseNames = expandCaseNames(args)
    caseNames.foreach { caseName =>
      processCase(caseName)
    }
    System.exit(0)
  }
}
