object DoOrthonormalTransform {
  def run(input:GrayImage, transform:OrthonormalTransform) : GrayImage = {
    val (m0, m1, staff, staffSeparationsMax) = (transform.m0, transform.m1,
      transform.staff, transform.staffSeparationsMax)
    val BoundingBox(minTargetX, maxTargetX, minTargetY, maxTargetY) =
      transform.bounds

    val squaredUp = new GrayImage(maxTargetX - minTargetX + 1 + 1,
                                  maxTargetY - minTargetY + 1 + 1)
    (0 until input.w).foreach { sourceX =>
      (0 until input.h).foreach { sourceY =>
        val v = input(sourceX, sourceY)

        val targetX = Ocr4Music.targetXFor(sourceX, sourceY, m0, m1, input.w) -
           minTargetX
        val targetY = Ocr4Music.targetYFor(sourceX, sourceY, staff, 
          staffSeparationsMax) - minTargetY

        squaredUp(targetX,     targetY)     = v
        squaredUp(targetX + 1, targetY)     = v
        squaredUp(targetX,     targetY + 1) = v
        squaredUp(targetX + 1, targetY + 1) = v
      }
    }
    squaredUp
  }
}
