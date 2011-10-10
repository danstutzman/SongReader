import java.lang.Math
import scala.util.Sorting

object FindVerticalSlices {
  def findEasyVerticalCuts(outer:BoundingBox, input:GrayImage,
      gapSmoothing:Int, ledgerLines:OrthonormalTransform) = {
    val maxVs = new Array[Int](outer.maxX + 1)
    val ledgerLineYs:List[Int] = List(8, 6, -6, -8).map { staffY =>
      ledgerLines.yForStaffY(staffY)
    }
    (outer.minX to outer.maxX).foreach { x =>
      var maxV = 0
      (outer.minY to outer.maxY).foreach { y =>
        val v = input(x, y)
        val onLedgerLine = ledgerLineYs.exists { y2 => Math.abs(y - y2) <= 2 }
        if (v > maxV && !onLedgerLine) {
          maxV = v
        }
      }
      maxVs(x) = maxV
    }

    val isFull = new Array[Boolean](outer.maxX + 1)
    (outer.minX to outer.maxX).foreach { x =>
      isFull(x) = false
    }

    val upperThreshold = 200
    val lowerThreshold = 128
    var isDescending = false
    (outer.minX to outer.maxX).foreach { x =>
      if (maxVs(x) >= upperThreshold)
        isDescending = true
      if (maxVs(x) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(x) = true
    }
    (outer.maxX to outer.minX by -1).foreach { x =>
      if (maxVs(x) >= upperThreshold)
        isDescending = true
      if (maxVs(x) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(x) = true
    }

    if (gapSmoothing > 0) {
      // Smooth over two-pixel gaps, for now
      var timeSinceLastWasFull = 9999
      var timeLastFull = 0
      (outer.minX to outer.maxX).foreach { x =>
        if (isFull(x)) {
          if (timeSinceLastWasFull <= gapSmoothing) {
            (timeLastFull to x - 1).foreach { x2 =>
              isFull(x2) = true
            }
          }
          timeSinceLastWasFull = 0
          timeLastFull = x
        } else {
          timeSinceLastWasFull += 1
        }
      }
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartX = outer.minX
    (outer.minX to outer.maxX + 1).foreach { x =>
      val prevFull = (if (x > outer.minX) isFull(x - 1) else false)
      val thisFull = (if (x < outer.maxX) isFull(x) else false)
      if (!prevFull && thisFull)
        boxStartX = x
      else if (prevFull && !thisFull)
        boxes = BoundingBox(boxStartX, x - 1, outer.minY, outer.maxY) :: boxes
    }
    boxes
  }

  def findEasyHorizontalCuts(outer:BoundingBox, input:GrayImage) = {
    val maxVs = new Array[Int](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      var maxV = 0
      (outer.minX to outer.maxX).foreach { x =>
        val v = input(x, y)
        if (v > maxV) {
          maxV = v
        }
      }
      maxVs(y) = maxV
    }

    val isFull = new Array[Boolean](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      isFull(y) = false
    }

    val upperThreshold = 128
    val lowerThreshold = 32
    var isDescending = false
    (outer.minY to outer.maxY).foreach { y =>
      if (maxVs(y) >= upperThreshold)
        isDescending = true
      if (maxVs(y) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(y) = true
    }
    (outer.maxY to outer.minY by -1).foreach { y =>
      if (maxVs(y) >= upperThreshold)
        isDescending = true
      if (maxVs(y) < lowerThreshold)
        isDescending = false
      if (isDescending)
        isFull(y) = true
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartY = outer.minY
    (outer.minY to outer.maxY + 1).foreach { y =>
      val prevFull = (if (y > outer.minY) isFull(y - 1) else false)
      val thisFull = (if (y < outer.maxY) isFull(y) else false)
      if (!prevFull && thisFull)
        boxStartY = y
      else if (prevFull && !thisFull)
        boxes = BoundingBox(outer.minX, outer.maxX, boxStartY, y - 1) :: boxes
    }
    boxes
  }

  def findHorizontalCuts(outer:BoundingBox, input:GrayImage) = {
    val isFull = new Array[Boolean](outer.maxY + 1)
    (outer.minY to outer.maxY).foreach { y =>
      isFull(y) = false
    }

    (outer.minY to outer.maxY).foreach { y =>
      val values = new Array[Int](outer.maxX + 1)
      (outer.minX to outer.maxX).foreach { x =>
        val v = input(x, y)
        values(x) = v
      }
      Sorting.quickSort(values)
      if (values(values.size - 1) >= 255)
        isFull(y) = true
    }

    var boxes:List[BoundingBox] = Nil
    var boxStartY = outer.minY
    (outer.minY to outer.maxY + 1).foreach { y =>
      val prevFull = (if (y > outer.minY) isFull(y - 1) else false)
      val thisFull = (if (y < outer.maxY) isFull(y) else false)
      if (!prevFull && thisFull)
        boxStartY = y
      else if (prevFull && !thisFull)
        boxes = BoundingBox(outer.minX, outer.maxX, boxStartY, y - 1) :: boxes
    }
    boxes
  }

  def cutOnVLines(
      boxes:List[BoundingBox], vlineXs:List[Int]) : List[BoundingBox] = {
    vlineXs match {
      case Nil =>
        boxes
      case x :: otherVLineXs =>
        val newBoxes = boxes.map { box =>
          if (x > box.minX && x < box.maxX) {
            List(BoundingBox(box.minX, x, box.minY, box.maxY),
                 BoundingBox(x, box.maxX, box.minY, box.maxY))
          } else {
            List(box)
          }
        }.foldLeft(List[BoundingBox]()) { _ ++ _ }
        cutOnVLines(newBoxes, otherVLineXs)
    }
  }

  def mergeAdjacent(points:List[Int]) = {
    var newPoints:List[Int] = Nil
    var inRun = false
    var runStart = 0
    var lastPoint = -999
    points.sorted.foreach { x =>
      if (inRun) {
        if (x - lastPoint >= 0 && x - lastPoint <= 1) {
          // do nothing
        } else {
          newPoints = (runStart + lastPoint) / 2 :: newPoints
          inRun = false
        }
      } else {
        if (x - lastPoint >= 0 && x - lastPoint <= 1) {
          inRun = true
          runStart = lastPoint
        } else {
          newPoints = x :: newPoints
        }
      }
      lastPoint = x
    }
    if (inRun) {
      newPoints = (runStart + lastPoint) / 2 :: newPoints
    }
    newPoints
  }

  def findYBounds(orthonormalImage:GrayImage, transform:OrthonormalTransform,
      staffName:String) = {
    val text = orthonormalImage.toColorImage
    var cutoff = new Array[Int](orthonormalImage.w)
    val staffY6 = transform.yForStaffY(6)
    (0 until orthonormalImage.w).foreach { x =>
      val v = orthonormalImage(x, staffY6) max
              orthonormalImage(x, staffY6 - 1) max
              orthonormalImage(x, staffY6 + 1)
      cutoff(x) = (v * 2) min 255
    }

    def updateCutoff(y:Int) {
      (0 until orthonormalImage.w).foreach { x =>
        val v = orthonormalImage(x, y)
        //if (cutoff(x) > 16 && v < cutoff(x) + 128) {
        //  if (v < cutoff(x))
        //    cutoff(x) = cutoff(x)*3/4 + v*1/4
        //}
        if (v >= cutoff(x) + 64) {
          //cutoff(x) = 0
          //demo(x, y) = ((v * 2) min 255, 0, 0)
          text(x, y) = (v - cutoff(x), 0, 0)
        }
        if (v < cutoff(x))
          cutoff(x) = cutoff(x)*7/8 + v*1/8
      }
    }

    (staffY6 until orthonormalImage.h).foreach { y =>
      updateCutoff(y)
    }

    val staffYNeg6 = transform.yForStaffY(-6)
    (0 until orthonormalImage.w).foreach { x =>
      val v = orthonormalImage(x, staffYNeg6) max
        orthonormalImage(x, staffYNeg6 - 1) max
        orthonormalImage(x, staffYNeg6 + 1)
      cutoff(x) = (v * 2) min 255
    }
    val ceiling = new GrayImage(orthonormalImage.w, orthonormalImage.h)
    (staffYNeg6 to 0 by -1).foreach { y =>
      updateCutoff(y)
    }

    var y = 0
    var consecutiveNonTextRows = 0
    while (y < staffYNeg6 && consecutiveNonTextRows < 5) {
      var sumAllV = 0
      var sumNonTextV = 0
      (0 until text.w).foreach { x =>
        val (allV, nonTextV, _) = text(x, y)
        sumAllV += (allV - 64) max 0
        sumNonTextV += (nonTextV - 64) max 0
      }
      if (sumNonTextV > 50 && sumNonTextV * 100 / sumAllV > 20)
        consecutiveNonTextRows += 1
      else
        consecutiveNonTextRows = 0
      y += 1
    }
    val minY = (y - consecutiveNonTextRows) max 0

    y = text.h - 1
    consecutiveNonTextRows = 0
    while (y > staffY6 && consecutiveNonTextRows < 5) {
      var sumAllV = 0
      var sumNonTextV = 0
      (0 until text.w).foreach { x =>
        val (allV, nonTextV, _) = text(x, y)
        sumAllV += (allV - 64) max 0
        sumNonTextV += (nonTextV - 64) max 0
      }
      if (sumNonTextV > 50 && sumNonTextV * 100 / sumAllV > 20)
        consecutiveNonTextRows += 1
      else
        consecutiveNonTextRows = 0
      y -= 1
    }
    val maxY = (y + consecutiveNonTextRows) min (text.h - 1)
    
    //(0 until text.w).foreach { x =>
    //  text(x, minY) = (text(x, minY)._1, text(x, minY)._2, 255)
    //  text(x, maxY) = (text(x, maxY)._1, text(x, maxY)._2, 255)
    //}
    //text.saveTo(new File("demos/staffy4.%s.png".format(staffName)))
    (minY, maxY)
  }

  def run(orthonormalImage:GrayImage, transform:OrthonormalTransform,
      vlines:List[VLine], staffName:String) = {
    val (minY, maxY) = findYBounds(orthonormalImage, transform, staffName)
    val input = orthonormalImage
    val wholeImage = BoundingBox(0, input.w - 1, minY, maxY)

    val verticalSlices = findEasyVerticalCuts(wholeImage, input, 2, transform)

    val vlineXs = vlines.map { vline =>
      transform.xForXIntercept(vline.xIntercept) }
    val adjacentVLines = mergeAdjacent(vlineXs)

    var boxToChildBoxes = Map[BoundingBox,List[BoundingBox]]()
    verticalSlices.foreach { parentBox =>
      val vlineXs = vlines.map { vline =>
        transform.xForXIntercept(vline.xIntercept) }
      val furtherCuts = findEasyVerticalCuts(parentBox, input, 0, transform)
      val childBoxes = cutOnVLines(furtherCuts, adjacentVLines)
      boxToChildBoxes = boxToChildBoxes.updated(parentBox, childBoxes)
    }
    boxToChildBoxes
  }
}
