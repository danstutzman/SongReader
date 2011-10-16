import java.io.File
import java.lang.Math

object FillBoxes {
  def saveWidths(boxes:List[BoundingBox], file:File) = {
    Ocr4Music.printToFile(file) { writer =>
      writer.println("MinX,MaxX,MinY,MaxY")
      boxes.foreach { box =>
        writer.println("%s,%s,%s,%s".format(
          box.minX, box.maxX, box.minY, box.maxY))
      }
    }
  }

  def prepareTemplate(templateName:String, templateW:Int, templateH:Int) :
      GrayImage = {
    val templatePath = new File("templates/%s.png".format(templateName))
    val bigTemplate =
      ColorImage.readFromFile(templatePath).toGrayImage.inverse
    val template = scaleTemplate(bigTemplate, templateW, templateH)
    template
  }

  def scaleTemplate(template:GrayImage, templateW:Int, templateH:Int) = {
    val templateScaled = new GrayImage(templateW, templateH)
    (0 until templateH).foreach { templateScaledY =>
      (0 until templateW).foreach { templateScaledX =>
        val y0 = templateScaledY * template.h / templateH
        val y1 =
          ((templateScaledY + 1) * template.h / templateH) min template.h
        val x0 = templateScaledX * template.w / templateW
        val x1 =
          ((templateScaledX + 1) * template.w / templateW) min template.w
        if (y1 > y0 && x1 > x0) {
          var sum = 0
          (y0 until y1).foreach { templateY =>
            (x0 until x1).foreach { templateX =>
              sum += template(templateX, templateY)
            }
          }
          val mean = sum / (y1 - y0) / (x1 - x0)
          templateScaled(templateScaledX, templateScaledY) = mean
        }
        else
          templateScaled(templateScaledX, templateScaledY) = 255
      }
    }
    templateScaled
  }

  def makeGradientImages(input:GrayImage, range:Int) : List[GrayImage] = {
    val gradientX     = new GrayImage(input.w, input.h)
    val gradientY     = new GrayImage(input.w, input.h)
    val gradientNorm  = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        var sumX = 0
        var sumY = 0
        var denom = 0
        (-range to range).foreach { xNeighbor =>
          (-range to range).foreach { yNeighbor =>
            val v = input(x + xNeighbor, y + yNeighbor)
            val xSign =
              if (xNeighbor == 0) 0 else (xNeighbor / Math.abs(xNeighbor))
            val ySign =
              if (yNeighbor == 0) 0 else (yNeighbor / Math.abs(yNeighbor))
            sumX += v * xSign
            sumY += v * ySign
            denom += 1
          }
        }
        var meanX = sumX / denom
        var meanY = sumY / denom

        val norm = Math.sqrt(meanX * meanX + meanY * meanY)
        if (norm > 0.0f) {
          gradientX(x, y) = (meanX * 100 / norm).intValue + 100
          gradientY(x, y) = (meanY * 100 / norm).intValue + 100
          gradientNorm(x, y) = norm.intValue
        } else {
          gradientX(x, y) = 127
          gradientY(x, y) = 127
          gradientNorm(x, y) = 0
        }
      }
    }
    List(gradientX, gradientY, gradientNorm)
  }

  def findImmovableInColumn(box:BoundingBox,
      inputGradientX:GrayImage, inputGradientY:GrayImage,
      templateGradientX:GrayImage, templateGradientY:GrayImage,
      input:GrayImage, template:GrayImage,
      templates:Map[String,GrayImage], templateName:String,
      fixedStaffY:Int, transform:OrthonormalTransform, staffName:String) :
      Option[TemplateMatch] = {
    val template = templates(templateName)
    val midX = (box.minX + box.maxX) / 2
    val minY = (box.minY + template.h/2) max (
      transform.yForStaffY(4) - template.h/2 - 20)
    val maxY = (box.maxY - template.h/2) max (
      transform.yForStaffY(-4) + template.h/2 + 20)
    val minX = box.minX + template.w/2 - 2
    val maxX = box.maxX - template.w/2 + 2
    var possiblePoints:List[(Int,Int,Int)] = Nil
    (minY to maxY).toList.foreach { y =>
      possiblePoints ++= (minX to maxX).toList.map { x =>
        (x, y, fixedStaffY)
      }
    }

    val results = findImmovable(
      inputGradientX, inputGradientY, templateGradientX, templateGradientY,
      input, template, possiblePoints, staffName)
    var maxResult = 0
    var argmaxNote:Option[TemplateMatch] = None
    (0 until results.size).foreach { i =>
      val (centerX, centerY, staffY) = possiblePoints(i)
      val result = results(i)
//if (templateName == "bass_clef") println(result)
      if (result > maxResult) {
        maxResult = result
        argmaxNote = Some(TemplateMatch(centerX, centerY,
          template.w, template.h, staffY, templateName, result))
      }
    }
    argmaxNote
  }

  def findNotesInColumn(box:BoundingBox, orthonormalImage:GrayImage,
      transform:OrthonormalTransform,
      templates:Map[String,GrayImage], templateName:String,
  finder:(GrayImage,GrayImage,List[(Int,Int,Int)],ColorImage,String)=>List[Int],
      donutDemo:ColorImage,
      staffName:String) : List[TemplateMatch] = {
    val template = templates(templateName)
    val midX = (box.minX + box.maxX) / 2
    val possibleStaffYs = (-8 to 8).toList
    val possiblePoints = possibleStaffYs.map { staffY =>
      (midX, transform.yForStaffY(staffY), staffY)
    }
    val results = finder(orthonormalImage, template,
      possiblePoints, donutDemo, staffName)
    var foundNotes:List[TemplateMatch] = Nil
    if (box.maxX - box.minX + 1 + 4 >= template.w) {
      (0 until results.size).foreach { i =>
        val (centerX, centerY, staffY) = possiblePoints(i)
        val result = results(i)
        foundNotes = TemplateMatch(centerX, centerY, template.w, template.h,
          staffY, templateName, result) :: foundNotes
      }
    }
    foundNotes
  }

  def findBlackHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      possiblePoints:List[(Int,Int,Int)], ignored:ColorImage, staffName:String) :
      List[Int] = {
    /*val leftEdge = Array(-1, 0, 1, -2, 0, 2, -1, 0, 1, 4)
    val rightEdge = Array(1, 0, -1, 2, 0, -2, 1, 0, -1, 4)
    val topEdge = Array(-1, -2, -1, 0, 0, 0, 1, 2, 1, 4)
    val bottomEdge = Array(1, 2, 1, 0, 0, 0, -1, -2, -1, 4)
    val blur = Array(1, 2, 1, 2, 4, 2, 1, 2, 1, 16)
    var guessFromEdge = new GrayImage(0, 0)
    //List(leftEdge, rightEdge, topEdge, bottomEdge, blur).foreach { edge =>
    List(leftEdge).foreach { edge =>
      val threshold = 50 // should be 250 for blur?
      val templateEdge =
        edgeDetection(rightSizeTemplate, edge).binarize(threshold)
      //templateEdge.saveTo(new File(
      //  "demos/smalltemplate.l.%s.png".format(staffName)))
      val inputEdge =
        edgeDetection(justNotes.inverse, edge).binarize(threshold)
      //inputEdge.saveTo(new File(
      //  "demos/edgedetect.%s.png".format(staffName)))
      guessFromEdge = slideTemplate(inputEdge, templateEdge) {
        (inputV, templateV) => inputV == 255 && templateV == 255
      }
    }*/

    val threshold = 128
    val templateDistance = ImageFilter.distance(rightSizeTemplate, threshold)
    //templateDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.t.%s.png".format(staffName)))
    val inputDistance = ImageFilter.distance(justNotes, threshold)
    //inputDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.i.%s.png".format(staffName)))

    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      var maxScore = 0
      var argmaxCenterX = 0
      var argmaxCenterY = 0
      (-1 to 1).foreach { xAdjust =>
        (-1 to 1).foreach { yAdjust =>
          val score = tryTemplateAt(inputDistance, templateDistance,
              centerX + xAdjust, centerY + yAdjust) {
            (inputV, templateV) =>
            inputV > 0 && templateV > 0 && inputV - templateV >= 0
          }
          if (score > maxScore) {
            maxScore = score
            argmaxCenterX = centerX + xAdjust
            argmaxCenterY = centerY + yAdjust
          }
        }
      }
      maxScore
    }
    scores
  }

  def findWhiteHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      possiblePoints:List[(Int,Int,Int)], donutDemo:ColorImage,
      staffName:String) = {
    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      countDonutHolePoints(justNotes,
        centerX - rightSizeTemplate.w/2, centerY - rightSizeTemplate.h/2,
        centerX + rightSizeTemplate.w/2, centerY + rightSizeTemplate.h/2,
        donutDemo)
    }
    scores
  }

/* def findAccidental(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      augmentedCaseName:String) = {
    val input = justNotes.inverse

    val inputLeftEdges = findLeftEdges(input)
    val templateLeftEdges = findLeftEdges(rightSizeTemplate)
    val leftEdgeMatch = slideTemplate(inputLeftEdges, templateLeftEdges) {
      (inputV, templateV) => inputV == 255 && templateV == 255
    }

    val inputRightEdges = findRightEdges(input)
    val templateRightEdges = findRightEdges(rightSizeTemplate)
    val rightEdgeMatch = slideTemplate(inputRightEdges, templateRightEdges) {
      (inputV, templateV) => inputV == 255 && templateV == 255
    }

    val combinedEdgeMatch =
        GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      leftEdgeMatch(x, y) + rightEdgeMatch(x, y)
    }
    val combinedEdgeMatch255 = combinedEdgeMatch.scaleValueToMax255

    val isDarkMatch = slideTemplate(input, rightSizeTemplate) {
      (inputV, templateV) => inputV > 128 && templateV == 255
    }
    val isDarkMatch255 = isDarkMatch.scaleValueToMax255

    val demo = GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      val r = combinedEdgeMatch255(x, y)
      val g = 0
      val b = isDarkMatch255(x, y)
      val rb = r * b / 255
      rb
    }
    demo.saveTo(new File(
      "demos/find_accidental.%s.png".format(augmentedCaseName)))

    val output = GrayImage.giveBrightnessPerPixel(input.w, input.h) { (x, y) =>
      combinedEdgeMatch(x, y) + isDarkMatch(x, y) / 2
    }
    output
  } */

  def findImmovable(
      inputGradientX:GrayImage, inputGradientY:GrayImage,
      templateGradientX:GrayImage, templateGradientY:GrayImage,
      input:GrayImage, template:GrayImage,
      possiblePoints:List[(Int,Int,Int)], staffName:String) = {
    val blackWhiteMatchThreshold = (0.75f * template.w * template.h).intValue
    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      val xScore = tryTemplateAt(inputGradientX, templateGradientX,
          centerX, centerY) { (inputV, templateV) =>
        inputV != 127 && templateV != 127 &&
          Math.abs(templateV - inputV) < 2
      }
      val yScore = tryTemplateAt(inputGradientY, templateGradientY,
          centerX, centerY) { (inputV, templateV) =>
        inputV != 127 && templateV != 127 &&
          Math.abs(templateV - inputV) < 2
      }
      val blackWhiteMatchScore = tryTemplateAt(input, template,
          centerX, centerY) { (inputV, templateV) =>
        (inputV > 128 && templateV > 128) ||
        (inputV < 128 && templateV < 128)
      }
      if (blackWhiteMatchScore > blackWhiteMatchThreshold) xScore * yScore
      else 0
    }
    scores
  }

  def tryTemplateAt(input:GrayImage, template:GrayImage,
      centerX:Int, centerY:Int)(scorer:(Int,Int)=>Boolean) : Int = {
    var score = 0
    (0 until template.w).foreach { templateX =>
      (0 until template.h).foreach { templateY =>
        val inputX = centerX + templateX - template.w/2
        val inputY = centerY + templateY - template.h/2
        val inputV = input(inputX, inputY)
        val templateV = template(templateX, templateY)
        val scoreDelta = (if (scorer(inputV, templateV)) 1 else 0)
        score += scoreDelta
      }
    }
    score
  }

  def findDiagonalLines(input:GrayImage, polarity:Boolean) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val diff1 = (input(x, y) - input(x - 2, y - 2))
        val diff2 = (input(x, y) - input(x + 2, y + 2))
        val newV =
          if (polarity && diff1 < 0 && diff2 < 0) -(diff1 + diff2) / 2
          else if (!polarity && diff1 > 0 && diff2 > 0) (diff1 + diff2) / 2
          else 0
        output(x, y) = newV
      }
    }
    output
  }

  def countDonutHolePoints(input:GrayImage, x0:Int, y0:Int, x1:Int, y1:Int,
      donutDemo:ColorImage) = {
    var numDonutHolePoints = 0
    //val donutHole = new GrayImage(x1 - x0 + 1, y1 - y0 + 1)
    val distance = 5
    val threshold = 96
    val threshold2 = 192
    (y0 until y1).foreach { y =>
      (x0 until x1).foreach { x =>
        val v = input(x, y)

        var hasBrighterLeftNeighbor = false
        ((x - distance) to (x - 1)).foreach { xNeighbor =>
          if (input(xNeighbor, y) > (threshold max v))
            hasBrighterLeftNeighbor = true
        }

        var hasBrighterRightNeighbor = false
        ((x + 1) to (x + distance)).foreach { xNeighbor =>
          if (input(xNeighbor, y) > (threshold max v))
            hasBrighterRightNeighbor = true
        }

        var hasBrighterTopNeighbor = false
        ((y - distance) to (y - 1)).foreach { yNeighbor =>
          if (input(x, yNeighbor) > (threshold max v))
            hasBrighterTopNeighbor = true
        }

        var hasBrighterBottomNeighbor = false
        ((y + 1) to (y + distance)).foreach { yNeighbor =>
          if (input(x, yNeighbor) > (threshold max v))
            hasBrighterBottomNeighbor = true
        }

        if (hasBrighterLeftNeighbor && hasBrighterRightNeighbor &&
            hasBrighterTopNeighbor  && hasBrighterBottomNeighbor &&
            v < threshold2) {
          //donutHole(x - x0, y - y0) = 255
          donutDemo(x, y) = (255, 0, 0)
          numDonutHolePoints += 1
        }
      }
    }

    numDonutHolePoints
  }

  def run(boxToChildBoxes:Map[BoundingBox,List[BoundingBox]],
      orthonormalImage:GrayImage, transform:OrthonormalTransform,
      staffName:String) = {
    val allChildBoxes =
      boxToChildBoxes.values.foldLeft(List[BoundingBox]()){ _++_ }
    saveWidths(allChildBoxes,
      new File("output/widths/%s.txt".format(staffName)))
    val widths = allChildBoxes.map { box => box.maxX - box.minX + 1 }.toList
    val orderedWidths = widths.filter { _ > 4 }.sorted
    val noteWidth = orderedWidths(orderedWidths.size * 3 / 4)

    val templates = Map(
      "white_head" -> prepareTemplate("white_head", noteWidth,
        Math.round(transform.staffSeparationsMax * 1.0f).intValue),
      "black_head" -> prepareTemplate("black_head", noteWidth * 10/10,
        Math.round(transform.staffSeparationsMax * 1.0f).intValue),
      "bass_clef" -> prepareTemplate("bass_clef", noteWidth * 24/10,
        Math.round(transform.staffSeparationsMax * 4.0f).intValue),
      "treble_clef" -> prepareTemplate("treble_clef", noteWidth * 20/10,
        Math.round(transform.staffSeparationsMax * 7.0f).intValue),
      "44" -> prepareTemplate("44", noteWidth * 12/10,
        Math.round(transform.staffSeparationsMax * 4.0f).intValue))

    val fClefStaffY = -2
    val gClefStaffY = 2
    val middleStaffY = 0

    val List(inputGradientX, inputGradientY, _) =
      makeGradientImages(orthonormalImage, 3)
    val List(templateTrebleGradientX, templateTrebleGradientY, _) =
      makeGradientImages(templates("treble_clef").addMargin(4), 3)
    val List(templateBassGradientX, templateBassGradientY, _) =
      makeGradientImages(templates("bass_clef").addMargin(4), 3)
    val List(template44GradientX, template44GradientY, _) =
      makeGradientImages(templates("44").addMargin(4), 3)

    var allFoundNotes:List[Set[TemplateMatch]] = Nil
    //val boxToAnnotatedStaffYs = matchBoxesToAnnotations(
    //  boxToChildBoxes.keys.toList, allAnnotationPoints, transform)
    val donutDemo = orthonormalImage.toColorImage
    val parentBoxes = boxToChildBoxes.keys.toList.sortBy { _.minX }
    val boxesOfTemplates = parentBoxes.map { parentBox =>
      printf("  Box at x=%04d,    ".format(parentBox.minX))
      val immovables:Set[TemplateMatch] = 
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          templateTrebleGradientX, templateTrebleGradientY,
          orthonormalImage, templates("treble_clef"),
          templates, "treble_clef", gClefStaffY, transform, staffName
          ).toSet ++
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          templateBassGradientX, templateBassGradientY,
          orthonormalImage, templates("bass_clef"),
          templates, "bass_clef", fClefStaffY, transform, staffName
          ).toSet ++
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          template44GradientX, template44GradientY,
          orthonormalImage, templates("44"),
          templates, "44", middleStaffY, transform, staffName).toSet
      val childBoxes =
        if (immovables.size == 0) {
          boxToChildBoxes(parentBox).map { box =>
            val newFoundNotes:Set[TemplateMatch] =
              findNotesInColumn(box, orthonormalImage, transform, templates,
                "white_head", findWhiteHeads, donutDemo, staffName).toSet ++
              findNotesInColumn(box, orthonormalImage, transform, templates,
                "black_head", findBlackHeads, donutDemo, staffName).toSet
            BoxOfTemplates(box, newFoundNotes, Nil)
          }
        } else Nil
      BoxOfTemplates(parentBox, immovables, childBoxes)
    }
    println() // since line wasn't ended before
    //donutDemo.saveTo(new File("demos/donut_demo.%s.png".format(staffName)))
    boxesOfTemplates
  }
}
