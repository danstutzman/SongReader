import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

object FindMidlines {
  def run(image:GrayImage, caseName:String) : ColorImage = {
    val output = new ColorImage(image.w, image.h)

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
  
          val (rOld, gOld, bOld) = output(x, y)
          if (r > rOld)
            output(x, y) = (r, staffSeparation, image(x, y))
        }
      }
    }
    output.saveTo(new File("demos/find12.%s.png".format(caseName)))
    output
  }
}
