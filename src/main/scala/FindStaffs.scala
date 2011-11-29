import java.io.File
import java.lang.Math
import scala.util.Random
import scala.util.Sorting

object FindStaffs {
  def highlightMinimums(image:GrayImage) : GrayImage = {
    val out = new GrayImage(image.w, image.h)
    val size = 1
    (0 until image.w).foreach { x =>
      (0 until image.h).foreach { y =>
        var max = 0
        var min = 255
        (-size to size).foreach { yDelta =>
          val v = image(x, y + yDelta)
          if (v > max) max = v
          if (v < min) min = v
        }
        val isMin = (image(x, y) == min)

        if (isMin) {
          val v = max - min
          out(x, y) = v
        } else {
          out(x, y) = 0
        }
      }
    }
    out
  }

  def quarterSize(image:GrayImage) = {
    val output = new GrayImage(image.w / 2, image.h / 2)
    (0 until output.w).foreach { x =>
      (0 until output.h).foreach { y =>
        output(x, y) = (
          image(x*2 - 2, y*2 - 2) *  1 +
          image(x*2 - 1, y*2 - 2) *  4 +
          image(x*2 + 0, y*2 - 2) *  6 +
          image(x*2 + 1, y*2 - 2) *  4 +
          image(x*2 + 2, y*2 - 2) *  1 +
          image(x*2 - 2, y*2 - 1) *  4 +
          image(x*2 - 1, y*2 - 1) * 16 +
          image(x*2 + 0, y*2 - 1) * 24 +
          image(x*2 + 1, y*2 - 1) * 16 +
          image(x*2 + 2, y*2 - 1) *  4 +
          image(x*2 - 2, y*2 + 0) *  6 +
          image(x*2 - 1, y*2 + 0) * 24 +
          image(x*2 + 0, y*2 + 0) * 36 +
          image(x*2 + 1, y*2 + 0) * 24 +
          image(x*2 + 2, y*2 + 0) *  6 +
          image(x*2 - 2, y*2 + 1) *  4 +
          image(x*2 - 1, y*2 + 1) * 16 +
          image(x*2 + 0, y*2 + 1) * 24 +
          image(x*2 + 1, y*2 + 1) * 16 +
          image(x*2 + 2, y*2 + 1) *  4 +
          image(x*2 - 2, y*2 + 2) *  1 +
          image(x*2 - 1, y*2 + 2) *  4 +
          image(x*2 + 0, y*2 + 2) *  6 +
          image(x*2 + 1, y*2 + 2) *  4 +
          image(x*2 + 2, y*2 + 2) *  1 +
          0) / 256
      }
    }
    output
  }

  def detectStaffs(image:GrayImage, caseName:String) : List[Staff] = {
    val demo = highlightMinimums(image)
    val thresholdX = 20
    val thresholdY = 5

    case class Point(val x:Int, val centerY:Int, val ys:List[Int],
      val ySpan:Int, threshold:Int, stackHeight:Int) {}

    case class Group(val points:List[Point], box:BoundingBox) {}

    def gatherPoints(threshold:Int, stackHeight:Int) : List[Point] = {
      var points = List[Point]()
      (0 until image.w).foreach { x =>
        var numBlacks = 0
        var vBeforeBlacks = 0
        var yBeforeBlacks = 0
        var numLinks = 0
        var ys = List[Int]()
        var staffBegin = 0
        (0 until image.h).foreach { y =>
          val v = demo(x, y)
          if (v <= 0) { // less than so it can detect blue (under zero) too
            numBlacks += 1
          } else {
            if ((numBlacks >= 2 && numBlacks <= 6) &&
                vBeforeBlacks >= threshold && v >= threshold) {
              numLinks += 1
              if (ys.size == 0)
                ys = List(yBeforeBlacks)
              ys = y :: ys
              if (numLinks >= stackHeight) {
                val centerY = (staffBegin + y) / 2
                points = Point(x, centerY, ys, y - staffBegin,
                  threshold, stackHeight) :: points
              }
            } else {
              numLinks = 0
              ys = List[Int]()
              staffBegin = y
            }
            vBeforeBlacks = v
            yBeforeBlacks = y
            numBlacks = 0
          }
        }
      }
      points
    }

    def expandExistingGroup(
        originalGroups:List[Group], allPoints:List[Point]) = {
      var expandedGroups = originalGroups.toArray
      allPoints.foreach { point =>
        var minMinDistance = 999999
        var argminGroupNum:Option[Int] = None
        originalGroups.zipWithIndex.foreach { groupAndGroupNum =>
          val (group, groupNum) = groupAndGroupNum
          if (point.x >= group.box.minX - thresholdX &&
              point.x <= group.box.maxX + thresholdX &&
              point.centerY >= group.box.minY - thresholdY &&
              point.centerY <= group.box.maxY + thresholdY) {
            var minDistance = 999999
            group.points.foreach { point2 =>
              val deltaX = Math.abs(point.x - point2.x)
              val deltaY = Math.abs(point.centerY - point2.centerY) 
              val distance = deltaX + deltaY
              if (distance < minDistance &&
                  deltaX <= thresholdX && deltaY <= thresholdY) {
                minDistance = distance
              }
            }
            if (minDistance < minMinDistance) {
              minMinDistance = minDistance
              argminGroupNum = Some(groupNum)
            }
          }
        }
        if (minMinDistance > 0) {
          argminGroupNum.foreach { groupNum =>
            val group = expandedGroups(groupNum)
            val newX0 = point.x min group.box.minX
            val newX1 = point.x max group.box.maxX
            val newY0 = point.centerY min group.box.minY
            val newY1 = point.centerY max group.box.maxY
            val newBounds = BoundingBox(newX0, newX1, newY0, newY1)
            expandedGroups(groupNum) = Group(point :: group.points, newBounds)
          }
        }
      }
      expandedGroups.toList
    }

    def expandGroupsFromPoints(
        originalGroups:List[Group], allPoints:List[Point]) = {
      var groups = originalGroups
      allPoints.foreach { point =>
        var nearbyGroups = List[Group]()
        groups.foreach { group =>
          if (point.x >= group.box.minX - thresholdX &&
              point.x <= group.box.maxX + thresholdX &&
              point.centerY >= group.box.minY - thresholdY &&
              point.centerY <= group.box.maxY + thresholdY) {
            val pointMatch = group.points.exists { point2 =>
              Math.abs(point.x - point2.x) <= thresholdX &&
              Math.abs(point.centerY - point2.centerY) <= thresholdY
            }
            if (pointMatch) {
              nearbyGroups = group :: nearbyGroups
            }
          }
        }
  
        val newX0 = nearbyGroups.foldLeft(point.x) { _ min _.box.minX }
        val newX1 = nearbyGroups.foldLeft(point.x) { _ max _.box.maxX }
        val newY0 = nearbyGroups.foldLeft(point.centerY) { _ min _.box.minY }
        val newY1 = nearbyGroups.foldLeft(point.centerY) { _ max _.box.maxY }
        val newBounds = BoundingBox(newX0, newX1, newY0, newY1)
        val newPoints = nearbyGroups.foldLeft(List(point)) { _ ++ _.points }
        groups = Group(newPoints, newBounds) ::
          groups.filter { !nearbyGroups.contains(_) }
      }

      groups.flatMap { group =>
        val matchingOriginalGroups = originalGroups.filter { originalGroup =>
          originalGroup.points.intersect(group.points).size > 0
        }
        if (matchingOriginalGroups.size == 0) {
          List(group) // keep new group
        } else if (matchingOriginalGroups.size == 1) {
          List(group) // keep larger group
        } else { // matches multiple original groups
          expandExistingGroup(matchingOriginalGroups, group.points.toList)
        }
      }.filter { _.points.size > 5 }
    }

    def drawGroups(groups:List[Group], threshold:Int, stackHeight:Int) = {
      val demo5 = new ColorImage(image.w, image.h)
      val random = new Random()
      groups.foreach { group =>
        val r = random.nextInt(192) + 63
        val g = random.nextInt(192) + 63
        val b = random.nextInt(192) + 63
        group.points.foreach { point =>
          (point.x-0 to point.x+0).foreach { neighborX =>
            //(point.y-point.wavelen5/2 to point.y+point.wavelen5/2).foreach {
            (point.centerY-0 to point.centerY+0).foreach {
                neighborY =>
              if (neighborX >= 0 && neighborX < demo5.w &&
                  neighborY >= 0 && neighborY < demo5.h) {
                demo5(neighborX, neighborY) = (r, g, b)
              }
            }
          }
        }
      }
      demo5
    }

    var groups = List[Group]()
    (7 to 2 by -1).foreach { threshold =>
      (4 to 1 by -1).foreach { stackHeight =>
        val allPoints = gatherPoints(threshold, stackHeight)
        if (stackHeight == 4)
          groups = expandGroupsFromPoints(groups, allPoints)
        else
          groups = expandExistingGroup(groups, allPoints)
      }
    }
    val demo5 = drawGroups(groups, 2, 1)
    demo5.saveTo(new File(
      "demos/newstaff1.%s.%d.%d.png".format(caseName, 2, 1)))

    val demo6 = image.toColorImage
    groups.foreach { group =>
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        var maxThreshold = 0
        var maxStackHeight = 0
        var argmaxPoints = List[Point]()
        column.foreach { point =>
          //if (point.stackHeight > maxStackHeight ||
          //   (point.stackHeight == maxStackHeight &&
          //    point.threshold > maxThreshold)) {
          if (point.threshold > maxThreshold ||
             (point.threshold == maxThreshold &&
              point.stackHeight > maxStackHeight)) {
            maxThreshold = point.threshold
            maxStackHeight = point.stackHeight
            argmaxPoints = List(point)
          } else if (point.threshold == maxThreshold &&
                     point.stackHeight == maxStackHeight) {
            argmaxPoints = point :: argmaxPoints
          }
        }
        argmaxPoints.foreach { point =>
          point.ys.foreach { y =>
            val (r, g, b) = demo6(point.x, y)
//            demo6(point.x, y) = (255, g, b)
          }
        }
      }
    }
//    demo6.saveTo(new File(
//      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

/*
    class HLine(var points:List[(Int,Int)]) {}

    val joinX = 3
    val joinY = 0
//    val demo6 = image.toColorImage
    val window = 10
    groups.foreach { group =>
      var oldHLines = List[HLine]()
      var currentHLines = List[HLine]()
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(List[Int]()) { _ ++ _.ys }
        ys.foreach { y =>
          val hline = currentHLines.find { hline =>
            x - hline.points.head._1 <= joinX &&
            Math.abs(y - hline.points.head._2) <= joinY
          }
          hline match {
            case Some(hline) =>
              hline.points = (x, y) :: hline.points
            case None =>
              currentHLines = new HLine(List((x, y))) :: currentHLines
          }
        }
        val (replaceCurrentHLines, addToOldHLines) = currentHLines.partition {
          x - _.points.head._1 <= joinX
        }
        currentHLines = replaceCurrentHLines
        oldHLines ++= addToOldHLines
      }
      var allHlines = (currentHLines ++ oldHLines).sortBy { -_.points.size }

      val longest = allHlines.head
      var hlineToIndex = Map[HLine,Int](longest -> 0)
      var unassigned = allHlines.filter { _ != longest }
      var assignedButNotNeighbors = List(longest)
      var assignedAndNeighbors = List[HLine]()
      while (assignedButNotNeighbors.size > 0) {
        val hline1 = assignedButNotNeighbors.head
        assignedButNotNeighbors = assignedButNotNeighbors.tail
        val min1 = hline1.points.map { _._1 }.min
        val max1 = hline1.points.map { _._1 }.max
        val y1 = hline1.points.map { _._2 }.max
        val index1 = hlineToIndex(hline1)
        unassigned.foreach { hline2 =>
          val min2 = hline2.points.map { _._1 }.min
          val max2 = hline2.points.map { _._1 }.max
          val y2 = hline2.points.map { _._2 }.max
          if (min1 <= max2 && max1 >= min2) {
            if (y1 - y2 >= 4 && y1 - y2 <= 6) {
              hlineToIndex = hlineToIndex.updated(hline2, index1 + 1)
              unassigned = unassigned.filter { _ != hline2 }
              assignedButNotNeighbors = assignedButNotNeighbors ++ List(hline2)
            } else if (y2 - y1 >= 4 && y2 - y1 <= 6) {
              hlineToIndex = hlineToIndex.updated(hline2, index1 - 1)
              unassigned = unassigned.filter { _ != hline2 }
              assignedButNotNeighbors = assignedButNotNeighbors ++ List(hline2)
            }
          }
        }
      }

      allHlines.foreach { hline =>
        val rgb = hlineToIndex.get(hline) match {
          case Some(i) => if (i % 2 == 0) (255, 0, 0) else (0, 255, 0)
          case None => (0, 0, 255)
        }
        hline.points.foreach { xy =>
          val (x, y) = xy
          demo6(x, y) = rgb
        }
      }*/

/*    val window = 10
    groups.foreach { group =>
      val xToYs = new Array[Set[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      val realMinY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 99999 else ys.min
      }.min
      val realMaxY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 0 else ys.max
      }.max

      if (group.box.minX < 88 && group.box.maxX > 88 &&
          realMaxY > 78 && realMinY < 78) {
        (-120 to 0).foreach { adjustment =>
          val yToScore = new Array[Int](realMaxY + window + 1)
          (78 to 190).foreach { x =>
            xToYs(x).foreach { y =>
              val adjustmentY = (x - 78) * adjustment / 120
              if (y - adjustmentY < 101) {
                yToScore(y - adjustmentY) += 1
                //yToScore(y - adjustmentY + 1) += 1
              }
            }
          }
        if (yToScore.foldLeft(0) { _ + _ } > 0) {
            (0 until yToScore.size).foreach { y =>
              val v = yToScore(y) * 2
              demo6(78 + adjustment, y) = (v, v, 0)
            }
          }
        }
      }
    }*/

/*
    groups.foreach { group =>
      val xToYs = new Array[List[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(List[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      val realMaxY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 0 else ys.max
      }.max

      val slopes = new Array[Int](group.box.maxX + 1)
      (group.box.minX to group.box.maxX - window).foreach { x0 =>
        val x1 = x0 + window
        var maxSumTop5 = 0
        var argmaxSlope = 0
        (2 to 2).foreach { slope =>
        //(-window to window).foreach { slope =>
          val yToScore = new Array[Int](realMaxY + window + 1)
          (0 until window).foreach { deltaX =>
            val x = x0 + deltaX
            xToYs(x).foreach { y =>
              val adjustedY = y - (slope * deltaX / window).intValue
              if (adjustedY >= 0) {
                yToScore(adjustedY) += 1
                yToScore(adjustedY + 1) += 1
              }
            }
          }
          val sumTop5 = yToScore.sorted.takeRight(5).sum
          if (sumTop5 > maxSumTop5) {
            maxSumTop5 = sumTop5
            argmaxSlope = slope
            (0 until realMaxY).foreach { y =>
              val v = (yToScore(y) * 5) min 255
              if (demo6(x0, y)._1 < v) {
                demo6(x0, y) = (v, v, 0)
              }
            }
          }
        }
        slopes(x0) = argmaxSlope
      }
    }
*/
/*
      (group.box.minX to group.box.maxX - window).foreach { x0 =>
        val neighborSlopes = (x0 until x0 + window).map { i => slopes(i) }
        val sumSlopes = neighborSlopes.foldLeft(0) { _ + _ }
        val meanSlope = sumSlopes / neighborSlopes.size
        val variance = neighborSlopes.map { slope =>
          (slope - meanSlope) * (slope - meanSlope)
        }.foldLeft(0) { _ + _ } / neighborSlopes.size.toFloat
        if (variance <= 2.0f) {
          (-10 to slopes(x0)).foreach { y =>
            demo6(x0 + window/2, realMaxY - 10 + y) = (255, 255, 0)
          }
        }
      }
*/
//    demo6.saveTo(new File(
//      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

//println(groups.map { _.points.filter { _.x == 83 }.sortBy { point =>
//  (point.threshold, point.stackHeight)
//}})

    var groupNumToXToYs = new Array[Array[Set[Int]]](groups.size)
    (0 until groups.size).foreach { groupNum =>
       val group = groups(groupNum)
       val xToYs = new Array[Set[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      groupNumToXToYs = groupNumToXToYs.updated(groupNum, xToYs)
    }

    // 999999 means uninitialized
    val groupNumToXToMinY = (0 until groups.size).toArray.map { groupNum =>
      (0 until image.w).map { x => 999999 }.toArray
    }
    // -1 means uninitialized
    val groupNumToXToMaxY = (0 until groups.size).toArray.map { groupNum =>
      (0 until image.w).map { x => -1 }.toArray
    }

    (0 until image.w).foreach { x =>
      var groupNumToYs = new Array[Set[Int]](groups.size)
      groups.zipWithIndex.foreach { groupAndGroupNum =>
        val (group, groupNum) = groupAndGroupNum
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        groupNumToYs(groupNum) = ys
      }

      (0 until image.h).foreach { y =>
        var minDistance = 999999
        var argmaxGroupNum:Option[Int] = None
        (0 until groups.size).foreach { groupNum =>
          groupNumToYs(groupNum).foreach { y2 =>
            val distance = Math.abs(y2 - y)
            if (distance < minDistance) {
              minDistance = distance
              argmaxGroupNum = Some(groupNum)
            }
          }
        }
        argmaxGroupNum.foreach { groupNum =>
          groupNumToXToMinY(groupNum)(x) = groupNumToXToMinY(groupNum)(x) min y
          groupNumToXToMaxY(groupNum)(x) = groupNumToXToMaxY(groupNum)(x) max y
        }
      }
    }

    val staffStrength = new GrayImage(image.w, image.h)
    val bestWavelen5 = new GrayImage(image.w, image.h)
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      (15 to 28).foreach { wavelen5 =>
        (group.box.minX until group.box.maxX).foreach { x =>
          val minY = groupNumToXToMinY(groupNum)(x)
          val maxY = groupNumToXToMaxY(groupNum)(x)
  
          (minY to maxY).foreach { centerY =>
            val insidePoints = List(-4, -2, 0, 2, 4).map { i =>
              demo(x, centerY + Math.floor(wavelen5 * i/10.0f).intValue) max
              demo(x, centerY + Math.floor(wavelen5 * i/10.0f).intValue + 1)
            }.toArray
            val meanInside = insidePoints.min
  
            val oldV = staffStrength(x, centerY)
            if (meanInside > oldV) {
              staffStrength(x, centerY) = meanInside
              bestWavelen5(x, centerY) = wavelen5
            }
          }
        }
      }
    }

    val howClose = 10 // number of neighbors to the left and right to consider
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

    val demo7 = staffStrength.scaleValueToMax255.toColorImage
    val groupNumToXToBestY = new Array[Array[Int]](groups.size)
    val groupNumToXToBestWavelen5 = new Array[Array[Int]](groups.size)
    val random = new Random()
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      val rgb = (random.nextInt(16) + 15,
                 random.nextInt(16) + 15,
                 random.nextInt(16) + 15)
      (group.box.minX to group.box.maxX).foreach { x =>
        val minY = groupNumToXToMinY(groupNum)(x)
        val maxY = groupNumToXToMaxY(groupNum)(x)
        (minY to maxY).foreach { y =>
          val rgbOld = demo7(x, y)
          demo7(x, y) = ((rgbOld._1 + rgb._1) min 255,
                         (rgbOld._2 + rgb._2) min 255,
                         (rgbOld._3 + rgb._3) min 255)
        }
      }

      val xToVYPairs = new Array[Array[(Int,Int)]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val minY = groupNumToXToMinY(groupNum)(x)
        val maxY = groupNumToXToMaxY(groupNum)(x)

        if (maxY - minY > 4) {
          var vyPairs = new Array[(Int,Int)](maxY - minY + 1)
          (minY to maxY).foreach { y =>
            vyPairs(y - minY) = (staffStrength(x, y), y)
          }
          Sorting.quickSort(vyPairs)
          xToVYPairs(x) = vyPairs
        }
      }

      val xToBestY = new Array[Int](group.box.maxX + 1)
      val xToBestWavelen5 = new Array[Int](group.box.maxX + 1)
      var threshold = staffStrength.data.max
      while (threshold > 0) {
        (group.box.minX to group.box.maxX).foreach { x =>
          val vyPairs = xToVYPairs(x)
          if (vyPairs != null && xToBestY(x) == 0) {
            val (max1V, max1Y) = vyPairs(vyPairs.size - 1)
            val (max2V, max2Y) = vyPairs(vyPairs.size - 2)
            val (max3V, max3Y) = vyPairs(vyPairs.size - 3)
            val (max4V, max4Y) = vyPairs(vyPairs.size - 4)
            val (maxV, bestY, nextBestV) =
              // if the 3 brightest are adjacent, average them and take middle
              if ((max1Y max max2Y max max3Y) -
                  (max1Y min max2Y min max3Y) == 2)
                ((max1V + max2V + max3V) / 3,
                 (max1Y min max2Y min max3Y) + 1,
                 max4V)
              // if the 2 brightest are adjacent, average them and take lower
              else if ((max1Y max max2Y) - (max1Y min max2Y) == 1)
                ((max1V + max2V) / 2, max1Y min max2Y, max3V)
              // otherwise pick the brightest
              else
                (max1V, max1Y, max2V)
            if (maxV - nextBestV >= threshold) {
              // now look at its neighbors to see if they form a line
              val (leftPoints, rightPoints) = getNeighborPoints(xToBestY, x)
              val errorYOverX =
                if (leftPoints.size + rightPoints.size > 1) {
                  val (slope, intercept) =
                    linearRegression(leftPoints ++ rightPoints)
                  val predictedY = slope * x + intercept
                  val errorY = Math.abs(predictedY - bestY)
                  val minXDistance = 
                    if (leftPoints.size > 0 && rightPoints.size > 0) {
                      Math.abs(leftPoints(leftPoints.size - 1)._1 - x) min
                        Math.abs(rightPoints(0)._1 - x)
                    } else if (leftPoints.size > 0) {
                      Math.abs(leftPoints(leftPoints.size - 1)._1 - x)
                    } else if (rightPoints.size > 0) {
                      Math.abs(rightPoints(0)._1 - x)
                    } else 999999
//println((errorY / minXDistance, errorY, minXDistance,
//  leftPoints, rightPoints, x, bestY))
                  errorY / minXDistance
                } else {
                  0.0
                }
              if (errorYOverX < 0.5) {
                xToBestY(x) = bestY
                xToBestWavelen5(x) = bestWavelen5(x, bestY)
                demo7(x, bestY) = (0, 255, 0)
              }
            } // end if meets threshold
          } // end if has point in x
        } // next x
        threshold -= 1
      } // next threshold
      groupNumToXToBestY(groupNum) = xToBestY
      groupNumToXToBestWavelen5(groupNum) = xToBestWavelen5
    } // next group


    // now fill in the gaps
/*
    val groupNumToFullXToBestY = new Array[Array[Int]](groups.size)
    val requiredDensity = 0.3f
    val groupNumToRealMinX = new Array[Int](groups.size)
    (0 until groups.size).foreach { i => groupNumToRealMinX(i) = 999999 }
    val groupNumToRealMaxX = new Array[Int](groups.size)
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      val xToBestY = groupNumToXToBestY(groupNum)
      val fullXToBestY = new Array[Int](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        if (xToBestY(x) == 0) {
          val (leftPoints, rightPoints) = getNeighborPoints(xToBestY, x)
          if (leftPoints.size > 0 && rightPoints.size > 0) {
            val leftmostX = leftPoints(0)._1
            val rightmostX = rightPoints(rightPoints.size - 1)._1
            if (leftPoints.size >= (x - leftmostX) * requiredDensity &&
                rightPoints.size >= (rightmostX - x) * requiredDensity) {
              val (slope, intercept) =
                linearRegression(leftPoints ++ rightPoints)
              val predictedY = Math.round(slope * x + intercept).intValue
              fullXToBestY(x) = predictedY
              groupNumToRealMinX(groupNum) = groupNumToRealMinX(groupNum) min x
              groupNumToRealMaxX(groupNum) = groupNumToRealMaxX(groupNum) max x
              demo7(x, predictedY) = (0, 0, 255)
            }
          }
        } else {
          fullXToBestY(x) = xToBestY(x)
        }
      }
      groupNumToFullXToBestY(groupNum) = fullXToBestY
    }*/
    demo7.scaleValueToMax255.saveTo(new File(
      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

    val denseGroupNums = (0 until groups.size).filter { groupNum =>
      val requiredDensity = 0.1f
      val numPoints = groupNumToXToBestY(groupNum).filter { _ != 0 }.size
      //println((numPoints, image.w, numPoints / image.w.toFloat))
      numPoints / image.w.toFloat >= requiredDensity
    }.toArray

    denseGroupNums.map { groupNum =>
      Staff(
        "%s-%02d".format(caseName, groupNum),
        groups(groupNum).box,
        groupNumToXToBestY(groupNum),
        groupNumToXToBestWavelen5(groupNum).map { _ / 5.0f }
      )
    }.toList
  }

  // returns (slope, intercept)
  def linearRegression(points:List[(Int,Int)]) : (Float, Float) = {
    val n = points.size
    var sumX = 0
    var sumY = 0
    var sumXY = 0
    var sumXX = 0
    points.foreach { xy =>
      val (x, y) = xy
      sumX += x
      sumY += y
      sumXY += x * y
      sumXX += x * x
    }
    val slope = (n * sumXY - sumX * sumY) /
                (n * sumXX - sumX * sumX).floatValue
    val intercept = (sumY - slope * sumX) / n.floatValue
    (slope, intercept)
  }

  def improveStaffs(staffs:List[Staff], image:GrayImage, demo:ColorImage) = {
    val howClose = 10 // number of neighbors to the left and right to consider
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

    staffs.map { staff =>
      val xToY = staff.midlineYs
      val xToWavelen5 = staff.staffSeparations.map { _ * 5.0f }

      var minSumDifference = 999999.0f
      var argmaxLeftWavelen5 = 0
      var argmaxRightWavelen5 = 0
      (15 to 130).foreach { leftWavelen5 =>
        (15 to 130).foreach { rightWavelen5 =>
          var sumDifference = 0.0f
          (0 until xToWavelen5.size).foreach { x =>
            val actualWavelen5 = xToWavelen5(x)
            if (actualWavelen5 != 0) {
              val progress = x / xToWavelen5.size.toFloat
              val predictedWavelen5 =
                leftWavelen5 + (rightWavelen5 - leftWavelen5) * progress
              sumDifference += Math.abs(predictedWavelen5 - actualWavelen5)
            }
          }
          if (sumDifference < minSumDifference) {
            minSumDifference = sumDifference
            argmaxLeftWavelen5 = leftWavelen5
            argmaxRightWavelen5 = rightWavelen5
          }
        }
      }

      val newXToYs = new Array[Int](xToY.size)
      val newXToWavelen5s = new Array[Float](xToWavelen5.size)
      (0 until xToY.size).foreach { x =>
        //demo(x, xToWavelen5(x)) = (0, 0, 255)
        //demo(x + 1, xToWavelen5(x)) = (0, 0, 255)
        val (leftPoints, rightPoints) = getNeighborPoints(xToY, x)
        if (leftPoints.size > 0 || rightPoints.size > 0) {
          val (slope, intercept) =
            linearRegression(leftPoints ++ rightPoints)
          val predictedCenterY = Math.round(slope * x + intercept).intValue
          val progress = x / xToY.size.toFloat
          val predictedWavelen5 = Math.round(argmaxLeftWavelen5 +
            (argmaxRightWavelen5 - argmaxLeftWavelen5) * progress).intValue
          //demo(x, predictedWavelen5) = (0, 0, 255)

          var minMaxV = 999999
          var argmaxCenterY = 0.0f
          var argmaxWavelen5 = 0.0f
          (predictedWavelen5 - 5 to predictedWavelen5 + 5).foreach { wavelen5 =>
            var centerY = predictedCenterY + -5.0f
            while (centerY < predictedCenterY + 5.0f) {
              val insidePoints = List(-4, -2, 0, 2, 4).map { i =>
                image(x, Math.floor(centerY + wavelen5 * i/10.0f).intValue) min
                image(x, Math.floor(centerY + wavelen5 * i/10.0f).intValue + 1)
              }.toArray
              val maxV = insidePoints.max
              if (maxV < minMaxV) {
                minMaxV = maxV
                argmaxCenterY = centerY
                argmaxWavelen5 = wavelen5
              }
  
              centerY += 0.2f
            }
          }

          List(-4, -2, 0, 2, 4).foreach { i =>
            demo(x, Math.round(argmaxCenterY + argmaxWavelen5 * i/10.0f
              ).intValue) = (255, 255, 0)
          }

          newXToYs(x) = Math.round(argmaxCenterY).intValue
          newXToWavelen5s(x) = argmaxWavelen5 / 5.0f
        }
        // otherwise, leave the values at 0
      } // next x
      Staff(staff.staffName, staff.bounds, newXToYs, newXToWavelen5s)
    } // next staff
  }

  def run(image:GrayImage, caseName:String) : List[Staff] = {
/*    val imageShrunken = caseName match {
      case "2" => quarterSize(quarterSize(image))
      case "4" => quarterSize(image)
      case "1320610762.M373157P72050Q0R91a6ef26faeb752b.macmini.0" => image
      case "1320611573.M358927P73155Q0Raf96d58b25a32902.macmini.0" => image
      case "1320613796.M254332P75976Q0R61fcb171586b3dba.macmini.0" =>
        quarterSize(quarterSize(image))
      case "1320618283.M681674P81510Q0R96e43bc997510706.macmini.0" =>
        quarterSize(quarterSize(image))
      case "1321227085.M56099P82785Q0R6b4534ebfa16e985.macmini.0" =>
        quarterSize(image)
      case "photo1" => quarterSize(image)
      case "photo2" => quarterSize(image)
      case "photo3" => quarterSize(image)
      case _ => image
    }*/

    def drawStaffs(staffs:List[Staff], rgb:(Int,Int,Int), out:ColorImage) {
      staffs.foreach { staff =>
        (0 until staff.midlineYs.size).foreach { x =>
          val y = staff.midlineYs(x)
          if (y != 0) {
            out(x, y) = rgb
          }
        }
      }
    }

    def multiplyStaffs(staffs:List[Staff], multiplier:Int) : List[Staff] = {
      staffs.map { staff =>
        val newXToY = new Array[Int](staff.midlineYs.size * (multiplier + 1))
        staff.midlineYs.zipWithIndex.foreach { yx =>
          val (oldY, oldX) = yx
          if (oldY != 0) {
            newXToY(oldX * multiplier + (multiplier / 2)) =
              oldY * multiplier + (multiplier / 2)
          }
        }
        val newStaffSeparations =
          new Array[Float](staff.staffSeparations.size * (multiplier + 1))
        staff.staffSeparations.zipWithIndex.foreach { yx =>
          val (oldY, oldX) = yx
          if (oldY != 0) {
            newStaffSeparations(oldX * multiplier + (multiplier / 2)) =
              oldY * multiplier
          }
        }
        val newBounds = BoundingBox(
          staff.bounds.minX * multiplier,
          staff.bounds.maxX * multiplier,
          staff.bounds.minY * multiplier,
          staff.bounds.maxY * multiplier
        )
        Staff(staff.staffName, newBounds, newXToY, newStaffSeparations)
      }
    }

    val demo = image.toColorImage
    val image16 = quarterSize(quarterSize(image))
    val image4 = quarterSize(image)
    val staffs16 = multiplyStaffs(detectStaffs(image16, caseName), 4)
    val staffs4 = multiplyStaffs(detectStaffs(image4, caseName), 2)
    val staffs1 =
      if (image.w + image.h < 1500)
        detectStaffs(image, caseName)
      else
        List[Staff]()
    val allStaffs = staffs16 ++ staffs4 ++ staffs1
    drawStaffs(allStaffs, (255, 0, 0), demo)
    val allStaffsNew = improveStaffs(allStaffs, image, demo)
    drawStaffs(allStaffsNew, (0, 255, 0), demo)
    demo.saveTo(new File("demos/all_staffs.%s.png".format(caseName)))

    allStaffsNew
  }
}
