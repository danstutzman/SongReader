import java.io.File
import java.lang.Math

object FindVSlopeRange {
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
      vhough:GrayImage, image:GrayImage, threshold:Int, caseName:String) = {

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
    demo.saveTo(new File("demos/vhoughnew.%s.png".format(caseName)))
    
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
    demo2.saveTo(new File("demos/vlinesshown.%s.png".format(caseName)))*/

    (argmaxYLeft, argmaxYRight)
  }
    
  def verticalHough(input:ColorImage, staffs:List[Staff], caseName:String) = {
    val hough = new GrayImage(input.w, 40)
    //val demo = new GrayImage(input.w, input.h)
    staffs.foreach { staff =>
      val bounds = staff.bounds
      (bounds.minY to bounds.maxY).foreach { inputY =>
        // add margin of 10 to avoid edges
        (bounds.minX + 10 until bounds.maxX - 10).foreach { inputX =>
          val v = input(inputX, inputY)
          (-20 until 20).foreach { mCents =>
            val inputXIntercept =
              Math.round(inputX - (mCents / 80.0f * inputY)).intValue
            if (inputXIntercept >= 0 && inputXIntercept < hough.w) {
              if (v == (0, 0, 127)) { // negative spot
                hough(inputXIntercept, mCents + 20) =
                  hough(inputXIntercept, mCents + 20) - 3
                //demo(inputX, inputY) = demo(inputX, inputY) - 255
              }
              else {
                hough(inputXIntercept, mCents + 20) =
                  hough(inputXIntercept, mCents + 20) + (v._1 / 63)
                //demo(inputX, inputY) = demo(inputX, inputY) + (v._1 / 63)
              }
            }
          }
        }
      }
    }
    //demo.saveTo(new File("demos/used4vhough.%s.png".format(caseName)))
    
    (0 until hough.h).foreach { y =>
      (0 until hough.w).foreach { x =>
        hough(x, y) = (hough(x, y) - 200) max 0
      }
    }

    //hough.scaleValueToMax255.saveTo(new File(
    //  "demos/vhough2.%s.png".format(caseName)))
    hough
  }

  def run(justNotes2:GrayImage, image:GrayImage, staffs:List[Staff],
      caseName:String) = {
    val justNotes2Distance = ImageFilter.distance(justNotes2, 200)
    val justNotes2Blurred = ImageFilter.edgeDetection(
      justNotes2, Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 9)).binarize(20)
    val demo = ColorImage.giveRGBPerPixel(image.w, image.h) { (x, y) =>
      if (justNotes2Distance(x, y) >= 3)
        (0, 0, 127)
      else {
        var v = justNotes2Blurred(x, y) / 4
        (v, v, v)
      }
    }
    val vhough = verticalHough(demo, staffs, caseName)
    val (atLeft, atRight) =
      findVLineInverseSlopeRangeGivenHough(vhough, image, 0, caseName)
    //demo.saveTo(new File("demos/stems.%s.png".format(caseName)))
   //justNotes2Blurred.saveTo(new File("demos/blurred.%s.png".format(caseName)))

    ((atLeft - 20) / 80.0f, (atRight - 20) / 80.0f)
  }
}
