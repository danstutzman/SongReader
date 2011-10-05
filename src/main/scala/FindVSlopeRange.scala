import java.lang.Math

object FindVSlopeRange {
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

  // Vertical lines (e.g. stems, measure lines, etc.) are often slightly
  // diagonal.  Traditional y/x slope acts weird around vertical lines,
  // so we're using x/y slope instead.
  // This method reports 1) the slope of the stem lines at the far left
  // of the page, as well as 2) the slope of the stem lines at the far
  // right of the page.  It's assumed that the slope of any vertical lines
  // between those points can be determined with linear interpolation.
  // So you can consider these two values as a line on the Hough transform
  // from (xIntercept=far left, slope=y1) to (xIntercept=far right, slope=y2).
  // (A line on the Hough transform corresponds to a series of nearly parallel
  // lines on the image).
  def findVLineInverseSlopeRangeGivenHough(
      vhough:GrayImage, image:GrayImage, threshold:Int, staffName:String) = {

    // Brute force linear regression: use a threshold on the Hough output
    // to find just the bright spots.  Then try all possible lines
    // with O(n^2) algorithm to keep the line with the brightest points on it.

    // Special cases:
    // 0) If no Hough bright spots pass the threshold, meaning that the Hough
    // transform couldn't find any long enough lines, then return the default
    // (reporting that lines are perfectly vertical).  Not too accurate,
    // but it's better than erroneously reporting very diagonal lines.

    // 1) If only one Hough bright spot passes the threshold, meaning that
    // there was one long line found in the image, but no other lines reliable
    // enough to determine how the perspective should vary the lines from the
    // left to the right of the image, then report a horizontal line (on
    // the Hough transform) through that bright spot (in other words, all 
    // near-vertical lines are parallel to the long one).

    var maxSum = 0
    var argmaxYLeft = vhough.h / 2 // default
    var argmaxYRight = vhough.h / 2 // default
    (0 until vhough.h).foreach { yLeft =>
      // Constrain yRight >= yLeft, because we assume that the stem lines
      // on the right side of the page are more counter-clockwise bent
      // than stem lines on the left side of the page.  Like this:  ///|||\\\
      // That should be the case if the camera is above and behind the paper.
      // Taking pictures from below or in front of the paper tricky to do
      // because of gravity and shadows, respectively.  If you could,
      // then stems lines would bend like this:  \\\|||/// and yRight should
      // be allowed to be < yLeft.
      (yLeft until vhough.h).foreach { yRight =>
        var sum = 0
        (0 until vhough.w).foreach { x =>
          val y = yLeft + (yRight - yLeft) * x / vhough.w
          val v = vhough(x, y)
          if (v > threshold)
            sum += (v - threshold)
          // the reason to subtract before adding to sum is that otherwise
          // the argmax line prefers to go through the Hough line where it's
          // thickest (the most points), not just where it's brightest
        }
    
        // Use >= instead of > so that if many lines have the same sum,
        // we'll pick the latest one, which happens to be the most horizontal,
        // since both yLeft and yRight will be maximized to pass through
        // whatever the bright point is.
        // The sum > 0 check is so the default doesn't get overridden
        if (sum > 0 && sum >= maxSum) {
          maxSum = sum
          argmaxYLeft = yLeft
          argmaxYRight = yRight
        }
      }
    }
    
    /*val demo = vhough.scaleValueToMax255.toColorImage
    (0 until demo.w).foreach { x =>
      val y = argmaxYLeft + (argmaxYRight - argmaxYLeft) * x / vhough.w
      val (r, g, b) = demo(x, y)
      demo(x, y) = (63 max r, g, b)
    }
    demo.saveTo(new File("demos/vhoughnew.%s.png".format(staffName)))
    
    var vlines:List[(Int,Int)] = Nil
    (0 until vhough.w).foreach { x =>
      val y = argmaxYLeft + (argmaxYRight - argmaxYLeft) * x / vhough.w
      if (vhough(x, y) > 2)
        vlines = (x, y) :: vlines
    }
    
    val demo2 = image.toColorImage
    vlines.foreach { vline =>
      val (xIntercept, inverseSlope40) = vline
      (0 until demo2.h).foreach { y =>
        val x =
          xIntercept + Math.round((inverseSlope40 - 20) / 80.f * y).intValue
        val (r, g, b) = demo2(x, y)
        demo2(x, y) = ((r + 63) min 255, g, b)
      }
    }
    demo2.saveTo(new File("demos/vlinesshown.%s.png".format(staffName)))*/

    (argmaxYLeft, argmaxYRight)
  }
    
  def verticalHough(input:ColorImage, staffName:String) = {
    val hough = new GrayImage(input.w, 40)
    (0 until input.h).foreach { inputY =>
      (10 until input.w - 10).foreach { inputX => // avoid edges
        val v = input(inputX, inputY)
        (-20 until 20).foreach { mCents =>
          val inputXIntercept =
            Math.round(inputX - (mCents / 80.0f * inputY)).intValue
          if (inputXIntercept >= 0 && inputXIntercept < hough.w) {
            if (v == (0, 0, 127)) // negative spot
              hough(inputXIntercept, mCents + 20) =
                hough(inputXIntercept, mCents + 20) - 3
            else
              hough(inputXIntercept, mCents + 20) =
                hough(inputXIntercept, mCents + 20) + (v._1 / 63)
          }
        }
      }
    }
    
    (0 until hough.h).foreach { y =>
      (0 until hough.w).foreach { x =>
        hough(x, y) = (hough(x, y) - 40) max 0
      }
    }

    //hough.scaleValueToMax255.saveTo(new File(
    //  "demos/vhough2.%s.png".format(staffName)))
    hough
  }

  def findVLineInverseSlopeRange(
      justNotes2:GrayImage, image:GrayImage, staffName:String) = {
    val justNotes2Distance = Ocr4Music.distance(justNotes2, 200)
    val justNotes2Blurred = Ocr4Music.edgeDetection(
      justNotes2, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(20)
    val demo = ColorImage.giveRGBPerPixel(image.w, image.h) { (x, y) =>
      if (justNotes2Distance(x, y) >= 3)
        (0, 0, 127)
      else {
        var v = justNotes2Blurred(x, y) / 4
        (v, v, v)
      }
    }
    val vhough = verticalHough(demo, staffName)
    val (atLeft, atRight) =
      findVLineInverseSlopeRangeGivenHough(vhough, image, 0, staffName)
    //demo.saveTo(new File("demos/stems.%s.png".format(staffName)))
   //justNotes2Blurred.saveTo(new File("demos/blurred.%s.png".format(staffName)))

    ((atLeft - 20) / 80.0f, (atRight - 20) / 80.0f)
  }

  // now that you know where to expect lines, do a better job of finding them
  def doVLineDetection(justNotes2:GrayImage, image:GrayImage,
      inverseSlopeRange:(Float,Float), staffName:String) = {
    val vEdges = findVerticalLines(justNotes2).binarize(10)
    //vEdges.saveTo(new File("demos/vedges.%s.png".format(staffName)))

    val justNotes2Blurred = Ocr4Music.edgeDetection(
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
    //demo3.saveTo(new File("demos/stems2.%s.png".format(staffName)))

    vlines
  }

  def run(justNotes2:GrayImage, image:GrayImage, staffName:String) = {
    findVLineInverseSlopeRange(justNotes2, image, staffName)
  }
}
