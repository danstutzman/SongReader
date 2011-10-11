object ImageFilter {
  def edgeDetection(input:GrayImage, matrix:Array[Int]) = {
    val output = new GrayImage(input.w, input.h)
    val denom = matrix(9)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val sum =
          (matrix(0) * input(x - 1, y - 1) +
           matrix(1) * input(x + 0, y - 1) +
           matrix(2) * input(x + 1, y - 1) +
           matrix(3) * input(x - 1, y + 0) +
           matrix(4) * input(x + 0, y + 0) +
           matrix(5) * input(x + 1, y + 0) +
           matrix(6) * input(x - 1, y + 1) +
           matrix(7) * input(x + 0, y + 1) +
           matrix(8) * input(x + 1, y + 1) +
           0) / denom
        output(x, y) = sum
      }
    }
    output
  }

  def distance(input:GrayImage, threshold:Int) = {
    // Distance from the top left
    val fromTL = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        if (input(x, y) < threshold)
          fromTL(x, y) = 0
        else {
          var candidates:List[Int] = List(input.w max input.h) // infinity

          if (y > 0 && x > 0)
            candidates = fromTL(x - 1, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y > 0)
            candidates = fromTL(x, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y > 0 && x < input.w - 1)
            candidates = fromTL(x + 1, y - 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (x > 0)
            candidates = fromTL(x - 1, y) + 1 :: candidates
          else
            candidates = 1 :: candidates

          fromTL(x, y) = candidates.min
        }
      }
    }

    // Distance from the bottom right
    val fromBR = new GrayImage(input.w, input.h)
    (input.h - 1 to 0 by -1).foreach { y =>
      (input.w - 1 to 0 by -1).foreach { x =>
        if (input(x, y) < threshold)
          fromBR(x, y) = 0
        else {
          var candidates:List[Int] = List(input.w max input.h) // infinity

          if (y < input.h - 1 && x < input.w - 1)
            candidates = fromBR(x + 1, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y < input.h - 1)
            candidates = fromBR(x, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (y < input.h - 1 && x > 0)
            candidates = fromBR(x - 1, y + 1) + 1 :: candidates
          else
            candidates = 1 :: candidates

          if (x < input.w - 1)
            candidates = fromBR(x + 1, y) + 1 :: candidates
          else
            candidates = 1 :: candidates

          fromBR(x, y) = candidates.min
        }
      }
    }

    val fromEither = new GrayImage(input.w, input.h)
    (0 until input.h).foreach { y =>
      (0 until input.w).foreach { x =>
        fromEither(x, y) = fromTL(x, y) min fromBR(x, y)
      }
    }
    fromEither
  }
}
