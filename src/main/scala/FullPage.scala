import java.io.File
import java.lang.Math
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

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

  def processCase(caseName:String) {
    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage

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

          val r = ((sortedWhiteValues(sortedWhiteValues.size * 1 / 2) -
            sortedBlackValues(sortedBlackValues.size - 1)) * 2) min 255
  
          val (rOld, gOld, bOld) = demo(x, y)
          demo(x, y) = (r max rOld, 0, image(x, y))
        }
      }
    }
    demo.saveTo(new File("demos/find12.%s.png".format(caseName)))
  }

  def main(args:Array[String]) {
    val caseNames = expandCaseNames(args)
    caseNames.foreach { caseName =>
      processCase(caseName)
    }
    System.exit(0)
  }
}
