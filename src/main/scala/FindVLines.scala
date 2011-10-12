import java.io.File
import java.lang.Math

object FindVLines {
  def findVerticalLines(input:GrayImage) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        val sum = (
          input(x - 4, y) * -4 +
          input(x - 3, y) * -2 +
          input(x - 2, y) *  0 +
          input(x - 1, y) *  1 +
          input(x + 0, y) *  2 +
          input(x + 1, y) *  1 +
          input(x + 2, y) *  0 +
          input(x + 3, y) * -2 +
          input(x + 4, y) * -4 +
          0) / 12
        output(x, y) = sum
      }
    }
    output
  }

  // now that you know where to expect lines, do a better job of finding them
  def run(justNotes2:GrayImage, image:GrayImage,
      inverseSlopeRange:(Float,Float), caseName:String) = {
    val vEdges = findVerticalLines(justNotes2).binarize(10)
    //vEdges.saveTo(new File("demos/vedges.%s.png".format(caseName)))

    val justNotes2Blurred = ImageFilter.edgeDetection(
      justNotes2, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(20)

    var vlines:List[VLine] = Nil
    val demo3 = image.toColorImage
    val (inverseSlopeAtLeft, inverseSlopeAtRight) = inverseSlopeRange
    (0 until image.w).foreach { xIntercept =>
      val progress = xIntercept / image.w.floatValue
      val inverseSlope = inverseSlopeAtLeft +
        (inverseSlopeAtRight - inverseSlopeAtLeft) * progress

      var sum = 0
      (0 until image.h).foreach { y =>
        val x = xIntercept + (inverseSlope * y).intValue
        val v = vEdges(x, y) / 255
        sum += v
      }

      val valuesSum = new Array[Int](image.h) // like a summed-area table in 1D
      var sum2 = 0
      (0 until image.h).foreach { y =>
        val x = xIntercept + (inverseSlope * y).intValue
        val v = justNotes2Blurred(x, y) / 255
        sum2 += v
        valuesSum(y) = sum2
      }

      var maxScore = 0
      var argmaxY0 = 0
      var argmaxY1 = 0
      (0 until (image.h - 1)).foreach { y0 =>
        ((y0 + 10) until image.h).foreach { y1 =>
          var sum = valuesSum(y1) - valuesSum(y0)
          val fullness = sum / (y1 - y0).floatValue
          val score = if (fullness > 0.99f && y1 - y0 >= 30) (y1 - y0) else 0
          if (score > maxScore) {
            maxScore = score
            argmaxY0 = y0
            argmaxY1 = y1
          }
        }
      }

      if (sum > 5) {
        //(0 until image.h).foreach { y =>
        (argmaxY0 until argmaxY1).foreach { y =>
          val x = xIntercept + (inverseSlope * y).intValue
          val (r, g, b) = demo3(x, y)
          val rNew = (r + 50) min 255
          demo3(x, y) = (rNew, g, b)
        }
        vlines = VLine(xIntercept, argmaxY0, argmaxY1) :: vlines
      }
    }
    //demo3.saveTo(new File("demos/stems2.%s.png".format(caseName)))

    vlines
  }
}
