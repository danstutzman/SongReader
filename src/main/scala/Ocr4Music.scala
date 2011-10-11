import com.twitter.json.Json
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.math.BigDecimal
import scala.util.Random
import scala.util.Sorting

object Colors {
  val underflow = (0, 0, 255) // blue (too cold)
  val overflow = (255, 0, 0) // red (too hot)
  val ansiEscapeToHighlightProgramOutput = "\u001b" + "[1;37m" // bright white
  val ansiEscapeNormal  = "\u001b" + "[0m"
}

case class BoundingBox (
  val minX:Int,
  val maxX:Int,
  val minY:Int,
  val maxY:Int
) {
  def toMap() : Map[String,Int] = {
    Map("minX" -> minX, "maxX" -> maxX, "minY" -> minY, "maxY" -> maxY)
  }
}
object BoundingBox {
  def fromMap(map:Map[String,Int]) : BoundingBox = {
    BoundingBox(map("minX"), map("maxX"), map("minY"), map("maxY"))
  }
}

case class Beam (
  val x0:Int,
  val x1:Int,
  val y0:Int,
  val y1:Int
) {
  def toMap() : Map[String,Int] = {
    Map("x0" -> x0, "x1" -> x1, "y0" -> y1, "y1" -> y1)
  }
}
object Beam {
  def fromMap(map:Map[String,Int]) : Beam = {
    Beam(map("x0"), map("x1"), map("y0"), map("y1"))
  }
}

case class LabeledPoint (
  val label:String,
  val x:Int,
  val y:Int,
  val staffY:Int
) {}

case class ExpectedNote (
  val label:String,
  val staffY:Int
) {}

case class Annotation (
  val points:List[LabeledPoint],
  val notes:List[Set[ExpectedNote]]
) {}

case class Performance (
  val correctNotes:Set[(Int,TemplateMatch)],
  val spuriousNotes:Set[(Int,TemplateMatch)],
  val missingNotes:Set[(Int,ExpectedNote)]
) {
  def numCorrect() = { correctNotes.size }
  def numSpurious() = { spuriousNotes.size }
  def numMissing() = { missingNotes.size }
  def precision() = { numCorrect.floatValue / (numCorrect + numSpurious) }
  def recall() = { numCorrect.floatValue / (numCorrect + numMissing) }
}

case class TemplateMatch (
  val x:Int,
  val y:Int,
  val w:Int,
  val h:Int,
  val staffY:Int,
  val templateName:String
) {}

case class TemplateSpec (
  val name:String,
  val widthInStaffLines:Double,
  val heightInStaffLines:Double,
  val finder:(GrayImage,GrayImage,String)=>GrayImage,
  val threshold:Double
) {}
 
case class VLine (
  val xIntercept:Int,
  val y0:Int,
  val y1:Int
) {
  def toMap() : Map[String,Int] = {
    Map("xIntercept" -> xIntercept, "y0" -> y0, "y1" -> y1)
  }
}
object VLine {
  def fromMap(map:Map[String,Int]) : VLine = {
    VLine(map("xIntercept"), map("y0"), map("y1"))
  }
}

case class Staff (
  val staffName:String,
  val bounds:BoundingBox,
  val midlineYs:Array[Int],
  val staffSeparations:Array[Float]
) {
  def toMap() : Map[String,Any] = {
    Map("staffName"        -> staffName,
        "bounds"           -> bounds.toMap,
        "midlineYs"        -> midlineYs,
        "staffSeparations" -> staffSeparations)
  }
}
object Staff {
  def fromMap(map:Map[String,Any]) : Staff = {
    val staffName = map("staffName").asInstanceOf[String]
    val bounds =
      BoundingBox.fromMap(map("bounds").asInstanceOf[Map[String,Int]])
    val midlineYs = map("midlineYs").asInstanceOf[List[Int]].toArray
    val staffSeparations = map("staffSeparations"
      ).asInstanceOf[List[BigDecimal]].map { _.toFloat }.toArray
    Staff(staffName, bounds, midlineYs, staffSeparations)
  }
}

case class OrthonormalTransform (
  val staff:Staff,
  val staffSeparationsMax:Float, // just here for speed
  val m0:Float, // v slope at left of image
  val m1:Float, // v slope at right of image
  val w:Int, // width of image
  val bounds:BoundingBox, // subtract minX and minY to find real coordinates
  val yForStaffY:Map[Int,Int],
  val xForXIntercept:Array[Int]
) {
  def transformXY = { (x:Int, y:Int) =>
    (Ocr4Music.targetXFor(x, y, m0, m1, w) - bounds.minX,
     Ocr4Music.targetYFor(x, y, staff, staffSeparationsMax) - bounds.minY)
  }
  def toMap() : Map[String,Any] = {
    Map("staff" -> staff.toMap,
        "staffSeparationsMax" -> staffSeparationsMax,
        "m0" -> m0, "m1" -> m1, "w" -> w,
        "bounds"         -> bounds.toMap,
        "yForStaffY"     -> yForStaffY.map { pair => List(pair._1, pair._2) },
        "xForXIntercept" -> xForXIntercept)
  }
}
object OrthonormalTransform {
  def fromMap(map:Map[String,Any]) : OrthonormalTransform = {
    val staff = Staff.fromMap(map("staff").asInstanceOf[Map[String,Any]])
    val staffSeparationsMax =
      map("staffSeparationsMax").asInstanceOf[BigDecimal].toFloat
    val m0 = map("m0").asInstanceOf[BigDecimal].toFloat
    val m1 = map("m1").asInstanceOf[BigDecimal].toFloat
    val w = map("w").asInstanceOf[Int]
    val bounds =
      BoundingBox.fromMap(map("bounds").asInstanceOf[Map[String,Int]])
    val yForStaffY = map("yForStaffY").asInstanceOf[List[List[Int]]].flatMap {
      pair => List((pair(0), pair(1)))
    }.toMap
    val xForXIntercept = map("xForXIntercept").asInstanceOf[List[Int]].toArray
    OrthonormalTransform(staff, staffSeparationsMax, m0, m1, w, bounds,
      yForStaffY, xForXIntercept)
  }
}

object Ocr4Music {
  def demoNotes(
      noteGroups:List[Set[TemplateMatch]], input:ColorImage, caseName:String) {
    val staffSeparation = 6
    val xSeparation = 18
    val darkYellow = (128, 128, 0)
    val brightYellow = (255, 255, 0)
    val staffHeight = 100
    val image = new ColorImage(input.w, staffHeight + input.h)

    // draw notes
    var staffX = -1
    noteGroups.foreach { noteGroup =>
      staffX += 1
    noteGroup.foreach { note =>
      val staffY = note.staffY
      val centerY = (staffHeight / 2) + (staffY * staffSeparation / 2).intValue
      (-8 to 8).foreach { x =>
        (-8 to 8).foreach { y =>
          if ((x * x) + 2 * (y * y) < 20) {
            image((staffX + 1) * xSeparation + x, centerY + y) =
              brightYellow
          }
        }
      }

      def drawLedgerLine(staffY:Int) {
        val ledgerLineY = ((staffY / 2).intValue * 2 * staffSeparation / 2) +
          (staffHeight / 2)
        (-8 to 8).foreach { x =>
          image((staffX + 1) * xSeparation + x, ledgerLineY) = darkYellow
        }
      }

      // draw ledger lines
      var staffYTemp = staffY
      while (staffYTemp >= 6) {
        drawLedgerLine(staffYTemp)
        staffYTemp -= 2
      }
      while (staffYTemp <= -6) {
        drawLedgerLine(staffYTemp)
        staffYTemp += 2
      }

      // draw staff (top-most so line vs. space notes are more obvious)
      (-4 to 4 by 2).foreach { staffYTemp =>
        (0 until image.w).foreach { x =>
          image(x, (staffHeight / 2) + (staffYTemp * staffSeparation / 2)) =
            darkYellow
        }
      }
    } // next note in noteGroup
    } // next noteGroup

    // copy input
    (0 to input.h).foreach { y =>
      (0 to input.w).foreach { x =>
        image(x, y + staffHeight) = input(x, y)
      }
    }

    image.saveTo(new File("demos/notes2.%s.png".format(caseName)))
  }

  def demoPointGroups(groups:List[Set[TemplateMatch]], input:GrayImage,
      caseName:String) {
    val demo = input.toColorImage
    groups.foreach { group =>
      val x0 = group.foldLeft(9999) { (accum, point) =>
        accum min (point.x - (point.w+1)/2)
      }
      val x1 = group.foldLeft(-9999) { (accum, point) =>
        accum max (point.x + (point.w+1)/2)
      }
      val y0 = group.foldLeft(9999) { (accum, point) =>
        accum min (point.y - (point.h+1)/2)
      }
      val y1 = group.foldLeft(-9999) { (accum, point) =>
        accum max (point.y + (point.h+1)/2)
      }

      (x0 to x1).foreach { x =>
        demo(x, y0) = (255, 0, 0)
        demo(x, y1) = (255, 0, 0)
      }
      (y0 to y1).foreach { y =>
        demo(x0, y) = (255, 0, 0)
        demo(x1, y) = (255, 0, 0)
      }
    }
    demo.saveTo(new File("demos/point_groups.%s.png".format(caseName)))
  }

  def groupTemplateMatches(matches:List[TemplateMatch]) = {
    var noteGroups:List[List[TemplateMatch]] = Nil
    var currentNoteGroup:List[TemplateMatch] = Nil
    var lastNoteX = -1
    val matchesSorted = matches.sortBy { _.x }
    matchesSorted.foreach { _match =>
      if (Math.abs(_match.x - lastNoteX) >= 20 && currentNoteGroup.size > 0) {
        noteGroups = currentNoteGroup :: noteGroups
        currentNoteGroup = Nil
      }

      currentNoteGroup = _match :: currentNoteGroup

      lastNoteX = _match.x
    }
    if (currentNoteGroup.size > 0)
      noteGroups = currentNoteGroup :: noteGroups

    noteGroups.reverse
  }

  def loadAnnotationJson(annotationString : String) : Annotation = {
    val annotationJson = Json.parse(annotationString)
    val points = annotationJson.asInstanceOf[List[Map[String,Any]]].map {
        pointJson => LabeledPoint(
          pointJson("type").asInstanceOf[String],
          pointJson("x").asInstanceOf[Int],
          pointJson("y").asInstanceOf[Int],
          pointJson("staffY").asInstanceOf[Int]
        )
      }

    var noteGroups:List[Set[ExpectedNote]] = Nil
    var currentNoteGroup = Set[ExpectedNote]()
    var lastNoteX = -999
    points.sortBy { _.x }.foreach { point =>
      if (Math.abs(point.x - lastNoteX) >= 20 && currentNoteGroup.size > 0) {
        noteGroups = currentNoteGroup :: noteGroups
        currentNoteGroup = Set[ExpectedNote]()
      }

      val templateName = point.label match {
        case "F"  => Some("bass_clef")
        case "G"  => Some("treble_clef")
        case "44" => Some("44")
        case "8"  => Some("black_head")
        case "4"  => Some("black_head")
        case "2"  => Some("white_head")
        case _    => None
      }
      templateName.foreach { name =>
        currentNoteGroup += ExpectedNote(name, point.staffY)
      }

      lastNoteX = point.x
    }
    if (currentNoteGroup.size > 0)
      noteGroups = currentNoteGroup :: noteGroups
    val notes = noteGroups.reverse

    Annotation(points, notes)
  }

  // Levenshtein minimum-edit-distance to determine best alignment
  // between detected note groups and ground truth note groups.
  // This ensures that a spurious or missed note only causes one
  // error instead of throwing off all the notes to the right.
  def calcPerformance(
      estimatedNotes:List[Set[TemplateMatch]],
      annotated:List[Set[ExpectedNote]]) = {
    //printf("init estimated: %s\n", estimatedNotes.map { _.map { _.staffY } })
    //printf("init annotated: %s\n", annotated)

    val estimated = estimatedNotes.map { _.map { _match =>
      ExpectedNote(_match.templateName, _match.staffY)
    } }
    var w = estimated.size
    var h = annotated.size
    var matrix = new Array[Array[Int]](h + 1)
    var backPointer = new Array[Array[(Int,Int)]](h + 1)
    (0 to h).foreach { y =>
      matrix(y) = new Array[Int](w + 1)
      backPointer(y) = new Array[(Int,Int)](w + 1)
    }

    case class Possibility (val direction:(Int,Int), val score:Int) {}
    (0 to h).foreach { y =>
      (0 to w).foreach { x =>
        var possibilities:List[Possibility] = Nil
        if (x > 0) {
          // If this noteGroup is totally spurious
          val scoreIncrease = estimated(x - 1).size
          val score = matrix(y)(x - 1) + scoreIncrease
          possibilities = Possibility((-1, 0), score) :: possibilities
        }
        if (y > 0) {
          // If this noteGroup is totally missing from prediction
          val scoreIncrease = annotated(y - 1).size
          val score = matrix(y - 1)(x) + scoreIncrease
          possibilities = Possibility((0, -1), score) :: possibilities
        }
        if (y > 0 && x > 0) {
          // If this predicted noteGroup corresponds with the next
          // annotated noteGroup
          var scoreIncrease = 0
          estimated(x - 1).foreach { i =>
            if (!(annotated(y - 1).contains(i)))
              scoreIncrease += 1
          }
          annotated(y - 1).foreach { i =>
            if (!(estimated(x - 1).contains(i)))
              scoreIncrease += 1
          }

          val score = matrix(y - 1)(x - 1) + scoreIncrease
          possibilities = Possibility((-1, -1), score) :: possibilities
        }

        if (x == 0 && y == 0) {
          matrix(y)(x) = 0
          backPointer(y)(x) = (0, 0)
        } else {
          var minScore = 99999
          var bestDirection = (0, 0)
          possibilities.foreach { possibility =>
            if (possibility.score < minScore) {
              minScore = possibility.score
              bestDirection = possibility.direction
            }
          }
          matrix(y)(x) = minScore
          backPointer(y)(x) = bestDirection
        }
      }
    }

    //matrix.foreach { row =>
    //  row.foreach { v =>
    //    printf("%3d", v)
    //  }
    //  printf("\n")
    //}

    case class PairedNoteGroup (
      val staffX:Int,
      val estimatedNotes:Set[TemplateMatch],
      val annotated:Set[ExpectedNote]
    ) {}
    var pairedNoteGroups:List[PairedNoteGroup] = Nil
    var x = w
    var y = h
    while (x > 0 || y > 0) {
      backPointer(y)(x) match {
        case (-1, 0) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimatedNotes(x - 1), Set()) ::
            pairedNoteGroups
          x -= 1
        case (0, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, Set(), annotated(y - 1)) ::
            pairedNoteGroups
          y -= 1
        case (-1, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimatedNotes(x - 1), annotated(y - 1)) ::
            pairedNoteGroups
          x -= 1
          y -= 1
      }
    }

    //pairedNoteGroups.foreach { group =>
    //  val PairedNoteGroup(_, estimated, annotated) = group
    //  printf("estimated: %s\n", estimated.map { _.staffY })
    //  printf("annotated: %s\n", annotated)
    //}

    var correctNotes:List[(Int,TemplateMatch)] = Nil // staffX, note
    var spuriousNotes:List[(Int,TemplateMatch)] = Nil // staffX, note
    var missingNotes:List[(Int,ExpectedNote)] = Nil // staffX, (type, staffY)
    pairedNoteGroups.foreach { pair =>
      val PairedNoteGroup(staffX, estimatedNotes, annotated) = pair
      estimatedNotes.foreach { note =>
        if (!(annotated.contains(ExpectedNote(note.templateName, note.staffY))))
          spuriousNotes = (staffX, note) :: spuriousNotes
        else
          correctNotes = (staffX, note) :: correctNotes
      }
      val estimatedStaffYs = estimatedNotes.map { _match =>
        ExpectedNote(_match.templateName, _match.staffY)
      }
      annotated.foreach { i =>
        if (!(estimatedStaffYs.contains(i)))
          missingNotes = (staffX, i) :: missingNotes
      }
    }

    Performance(correctNotes.toSet, spuriousNotes.toSet, missingNotes.toSet)
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

  def scaleTemplateColor(template:ColorImage, templateW:Int, templateH:Int) = {
    val templateScaled = new ColorImage(templateW, templateH)
    (0 until templateH).foreach { templateScaledY =>
      (0 until templateW).foreach { templateScaledX =>
        val y0 = templateScaledY * template.h / templateH
        val y1 =
          ((templateScaledY + 1) * template.h / templateH) min template.h
        val x0 = templateScaledX * template.w / templateW
        val x1 =
          ((templateScaledX + 1) * template.w / templateW) min template.w
        if (y1 > y0 && x1 > x0) {
          var (rSum, gSum, bSum) = (0, 0, 0)
          (y0 until y1).foreach { templateY =>
            (x0 until x1).foreach { templateX =>
              val (r, g, b) = template(templateX, templateY)
              rSum += r
              gSum += g
              bSum += b
            }
          }
          val rMean = rSum / (y1 - y0) / (x1 - x0)
          val gMean = gSum / (y1 - y0) / (x1 - x0)
          val bMean = bSum / (y1 - y0) / (x1 - x0)
          templateScaled(templateScaledX, templateScaledY) =
            (rMean, gMean, bMean)
        }
        else
          templateScaled(templateScaledX, templateScaledY) = (0, 0, 0)
      }
    }
    templateScaled
  }

  def drawTemplateMatch(
      _match:TemplateMatch, output:ColorImage, template:GrayImage,
      rgb:(Int,Int,Int)) {
    val templateScaled = scaleTemplate(template, _match.w, _match.h)
    val (templateR, templateG, templateB) = rgb
    (0 until templateScaled.h).foreach { templateScaledY =>
      (0 until templateScaled.w).foreach { templateScaledX =>
        val templateV = templateScaled(templateScaledX, templateScaledY)
        val demoX = (_match.x + templateScaledX - templateScaled.w / 2)
        val demoY = (_match.y + templateScaledY - templateScaled.h / 2)
        val (r, g, b) = output(demoX, demoY)
        if (templateV >= 128)
          output(demoX, demoY) = rgb
      }
    }
  }

  def dedupe(points:List[TemplateMatch]) : List[TemplateMatch] = {
    if (points.isEmpty)
      points
    else
      points.head :: dedupe(
        for (x <- points.tail if x.staffY != points.head.staffY) yield x)
  }

  def sumTemplate(input:GrayImage) : GrayImage = {
    val sum = new GrayImage(input.w, input.h)

    (0 until sum.h).foreach { y =>
      (0 until sum.w).foreach { x =>
        sum(x, y) = input(x, y)
      }
    }

    (1 until sum.h).foreach { y =>
      (0 until sum.w).foreach { x =>
        sum(x, y) = sum(x, y) + sum(x, y - 1)
      }
    }

    (0 until sum.h).foreach { y =>
      (1 until sum.w).foreach { x =>
        sum(x, y) = sum(x, y) + sum(x - 1, y)
      }
    }

    sum
  }

  // returns area of overlap divided by total area of two points
  def overlapAmount(point1:TemplateMatch, point2:TemplateMatch) = {
    val (x10, x11) = (point1.x - point1.w/2.0f, point1.x + point1.w/2.0f)
    val (y10, y11) = (point1.y - point1.h/2.0f, point1.y + point1.h/2.0f)
    val (x20, x21) = (point2.x - point2.w/2.0f, point2.x + point2.w/2.0f)
    val (y20, y21) = (point2.y - point2.h/2.0f, point2.y + point2.h/2.0f)
    val (x30, x31) = (x10 max x20, x11 min x21)
    val (y30, y31) = (y10 max y20, y11 min y21)
    val (overlapW, overlapH) = ((x31 - x30) max 0, (y31 - y30) max 0)
    (overlapW * overlapH).floatValue /
      ((point1.w min point2.w) * (point1.h min point2.h))
  }

  def groupOverlappingPoints(points:List[TemplateMatch]) = {
    var groups:List[Set[TemplateMatch]] = Nil
    points.foreach { point =>
      groups.filter { group =>
        group.exists { overlapAmount(point, _) >= 0.1f }
      } match {
        case Nil =>
          groups = Set(point) :: groups
        case oneGroup :: Nil =>
          groups = (oneGroup + point) :: groups.filter { _ != oneGroup }
        case manyGroups =>
          val merged = manyGroups.reduceLeft { _ ++ _ } ++ Set(point)
          groups = merged :: groups.filter { !manyGroups.contains(_) }
      }
    }
    groups
  }

  def listNonOverlappingAlternatives(points:List[TemplateMatch])
      : Set[Set[TemplateMatch]] = {
    points match {
      case Nil => Set(Set[TemplateMatch]())
      case point :: otherPoints =>
        val (overlapping, notOverlapping) = otherPoints.partition { otherPoint=>
          overlapAmount(point, otherPoint) >= 0.3f
        }
        val ifPointKept = listNonOverlappingAlternatives(
            notOverlapping).map { set => set ++ Set(point) }
        if (overlapping.isEmpty)
          ifPointKept
        else
          ifPointKept ++ listNonOverlappingAlternatives(otherPoints)
    }
  }

  def demoAlternatives(overlappingPointGroups:List[Set[TemplateMatch]],
      input:GrayImage, caseName:String) {
    val demo2 = input.toColorImage
    overlappingPointGroups.foreach { group =>
      val alternatives = listNonOverlappingAlternatives(group.toList)
      var i = 0
      alternatives.foreach { group2 =>
        val color = i match {
          case 0 => (255,0,0)
          case 1 => (0,255,0)
          case 2 => (0,0,255)
          case _ => (255,255,255)
        }

        // for example: gray + red drawn lightly = reddish gray
        def drawLightly(x:Int, y:Int) = {
          val (r, g, b) = demo2(x, y)
          demo2(x, y) = ((r + color._1) min 255,
                         (g + color._2) min 255,
                         (b + color._3) min 255)
        }

        group2.foreach { point =>
          val (x0, x1) = (point.x - (point.w+1)/2, point.x + (point.w+1)/2)
          val (y0, y1) = (point.y - (point.h+1)/2, point.y + (point.h+1)/2)
          (x0 to x1).foreach { x =>
            drawLightly(x, y0)
            drawLightly(x, y1)
          }
          (y0 to y1).foreach { y =>
            drawLightly(x0, y)
            drawLightly(x1, y)
          }
        }

        i += 1
      }
    }
    demo2.saveTo(new File("demos/alternatives.%s.png".format(caseName)))
  }

  def demoBounds(baseImage:ColorImage, bounds:List[BoundingBox],
      caseName:String) {
    val demo = baseImage.copy
    bounds.foreach { bound =>
      val color = (255, 0, 0)
      (bound.minX to bound.maxX).foreach { x =>
        demo(x, bound.minY) = color
        demo(x, bound.maxY) = color
      }
      (bound.minY to bound.maxY).foreach { y =>
        demo(bound.minX, y) = color
        demo(bound.maxX, y) = color
      }
    }
    demo.saveTo(new File("demos/bounds.%s.png".format(caseName)))
  }

  def filterImage(input:GrayImage)(
      filter:(GrayImage,Int,Int)=>Int) : GrayImage = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        output(x, y) = filter(input, x, y)
      }
    }
    output
  }

  def findLeftEdges(input:GrayImage) = {
    filterImage(input) { (input, x, y) =>
      if (input(x - 1, y) - input(x + 1, y) > 50) 255
      else 0
    }
  }

  def findRightEdges(input:GrayImage) = {
    filterImage(input) { (input, x, y) =>
      if (input(x - 1, y) - input(x + 1, y) < -50) 255
      else 0
    }
  }

  def slideTemplate(input:GrayImage, template:GrayImage)(
      scorer:(Int,Int)=>Boolean) = {
    val output = new GrayImage(input.w, input.h)
    (0 until input.w).foreach { inputX =>
      (0 until input.h).foreach { inputY =>
        val inputV = input(inputX, inputY)
        (0 until template.w).foreach { templateX =>
          (0 until template.h).foreach { templateY =>
            val templateV = template(templateX, templateY)
            val predictedCenterX = inputX - (templateX - template.w/2)
            val predictedCenterY = inputY - (templateY - template.h/2)
            if (predictedCenterX >= 0 && predictedCenterX < output.w &&
                predictedCenterY >= 0 && predictedCenterY < output.h &&
                scorer(inputV, templateV)) {
              output(predictedCenterX, predictedCenterY) =
                output(predictedCenterX, predictedCenterY) + 1
            }
          }
        }
      }
    }
    output
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

  def findWhiteHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      possiblePoints:List[(Int,Int,Int)], donutDemo:ColorImage,
      caseName:String) = {
    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      countDonutHolePoints(justNotes,
        centerX - rightSizeTemplate.w/2, centerY - rightSizeTemplate.h/2,
        centerX + rightSizeTemplate.w/2, centerY + rightSizeTemplate.h/2,
        donutDemo)
    }
    scores
  }

  def findAccidental(justNotes:GrayImage, rightSizeTemplate:GrayImage,
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

  def findImmovable(
      inputGradientX:GrayImage, inputGradientY:GrayImage,
      templateGradientX:GrayImage, templateGradientY:GrayImage,
      input:GrayImage, template:GrayImage,
      possiblePoints:List[(Int,Int,Int)], caseName:String) = {
    val blackWhiteMatchThreshold = (0.75f * template.w * template.h).intValue
    val scores = possiblePoints.map { possiblePoint =>
      val (centerX, centerY, staffY) = possiblePoint
      var maxScore = 0
      var argmaxCenterX = 0
      var argmaxCenterY = 0
      (-1 to 1).foreach { xAdjust =>
        (-1 to 1).foreach { yAdjust =>
          val xScore = tryTemplateAt(inputGradientX, templateGradientX,
              centerX + xAdjust, centerY + yAdjust) { (inputV, templateV) =>
            inputV != 127 && templateV != 127 &&
              Math.abs(templateV - inputV) < 2
          }
          val yScore = tryTemplateAt(inputGradientY, templateGradientY,
              centerX + xAdjust, centerY + yAdjust) { (inputV, templateV) =>
            inputV != 127 && templateV != 127 &&
              Math.abs(templateV - inputV) < 2
          }
          val blackWhiteMatchScore = tryTemplateAt(input, template,
              centerX + xAdjust, centerY + yAdjust) { (inputV, templateV) =>
            (inputV > 128 && templateV > 128) ||
            (inputV < 128 && templateV < 128)
          }
          val score = xScore * yScore
          if (score > maxScore &&
              blackWhiteMatchScore > blackWhiteMatchThreshold) {
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

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def readFile(f: java.io.File) = {
    val source = scala.io.Source.fromFile(f)
    val contents = source.mkString
    source.close()
    contents
  }

  def readOrGenerate[T](file:File, save:((T,File)=>Unit), restore:(File=>T))(
      generator:()=>T) = {
    if (!file.exists()) {
      printf("Generating %s...\n", file.getPath())
      val output = generator()
      save(output, file)
    } else {
      printf("Loading    %s...\n", file.getPath())
    }
    restore(file)
  }

  def findBlackHeads(justNotes:GrayImage, rightSizeTemplate:GrayImage,
      possiblePoints:List[(Int,Int,Int)], ignored:ColorImage, caseName:String) :
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
      //  "demos/smalltemplate.l.%s.png".format(caseName)))
      val inputEdge =
        edgeDetection(justNotes.inverse, edge).binarize(threshold)
      //inputEdge.saveTo(new File(
      //  "demos/edgedetect.%s.png".format(caseName)))
      guessFromEdge = slideTemplate(inputEdge, templateEdge) {
        (inputV, templateV) => inputV == 255 && templateV == 255
      }
    }*/

    val threshold = 128
    val templateDistance = distance(rightSizeTemplate, threshold)
    //templateDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.t.%s.png".format(caseName)))
    val inputDistance = distance(justNotes, threshold)
    //inputDistance.scaleValueToMax255.saveTo(new File(
    //  "demos/distance.i.%s.png".format(caseName)))

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

  def gleanPoints(detected:GrayImage, metrics:Metrics,
      yCorrection:Array[Float], templateW:Int, templateH:Int,
      threshold:Int, templateName:String) = {
    //val simplified = image.copy
    var points:List[TemplateMatch] = Nil
    (-8 to 8).foreach { staffY =>
      var maxV = 0
      var argmaxX = 0
      var argmaxY = 0
      var argmaxStaffY = 0
      (0 until detected.w).foreach { x =>
        val xCentered = x - (detected.w / 2)
        val a = metrics.a
        val staffY0 = staffY - 0.5f
        val staffY1 = staffY + 0.5f
        val b0 = metrics.b + (staffY0 / 2.0f * metrics.bSpacing)
        val b1 = metrics.b + (staffY1 / 2.0f * metrics.bSpacing)
        val c0 = metrics.c + (staffY0 / 2.0f * metrics.cSpacing)
        val c1 = metrics.c + (staffY1 / 2.0f * metrics.cSpacing)
        val y0 = Math.round((a * xCentered * xCentered + b0 * xCentered +
          c0) + yCorrection(x) + (detected.h / 2)).intValue
        val y1 = Math.round((a * xCentered * xCentered + b1 * xCentered +
          c1) + yCorrection(x) + (detected.h / 2)).intValue

        var hasPointAboveThreshold = false
//          (y0 until y1).foreach { y =>
val y = (y0 + y1) / 2
          val v = detected(x, y)
          if (v > threshold) {
            hasPointAboveThreshold = true
            if (v > maxV) {
              maxV = v
              argmaxX = x
              //argmaxY = y
              argmaxY = (y0 + y1) / 2
              argmaxStaffY = staffY
            }
          }
//          }
        if (!hasPointAboveThreshold && maxV != 0) {
          //simplified(argmaxX, argmaxY) = 255
          points = TemplateMatch(argmaxX, argmaxY, templateW, templateH,
            argmaxStaffY, templateName) :: points
          maxV = 0
        }

        //if (y0 >= 0 && y0 < simplified.h)
        //  simplified(x, y0) = 0
      }
    }
    points
  }

  def chooseBestOverlappingSets(
      bounds:BoundingBox,
      overlappingPointGroup:Set[TemplateMatch],
      templates:Map[String,GrayImage], orthonormalImage:GrayImage) :
      Set[TemplateMatch] = {
    val alternatives =
      listNonOverlappingAlternatives(overlappingPointGroup.toList)

    var bestAlternative = alternatives.toList(0)
    var bestI = 0
    var maxScore = -999999
    var i = 0
    alternatives.foreach { pointGroup =>
      var proposal = new ColorImage(bounds.maxX - bounds.minX + 1,
                                   bounds.maxY - bounds.minY + 1)
      (0 until proposal.w).foreach { x =>
        (0 until proposal.h).foreach { y =>
          var b = orthonormalImage(x + bounds.minX, y + bounds.minY)
          proposal(x, y) = (0, 0, b)

        }
      }
      pointGroup.foreach { point =>
        val template = templates(point.templateName)
        (0 until template.h).foreach { templateY =>
          (0 until template.w).foreach { templateX =>
            val x = (point.x - point.w/2) + templateX - bounds.minX
            val y = (point.y - point.h/2) + templateY - bounds.minY
            val (r, g, b) = proposal(x, y)
            proposal(x, y) = (r max template(templateX, templateY), g, b)
          }
        }
      }
      //proposal.saveTo(new File(
      //  "demos/proposal.%03d.%02d.png".format(bounds.minX, i)))

      var diff = 0
      (0 until proposal.w).foreach { x =>
        (0 until proposal.h).foreach { y =>
          diff += Math.abs(proposal(x, y)._1 -
            orthonormalImage(x + bounds.minX, y + bounds.minY))
        }
      }
      val score = -diff
        
      if (score > maxScore) {
        maxScore = score
        bestAlternative = pointGroup
        bestI = i
      }

      i += 1
    }
    //println("best for %03d is %d".format(bounds.minX, bestI))
    bestAlternative
  }

  def demoBeams(beams:List[Beam], image:GrayImage, caseName:String) {
    val demo2 = image.toColorImage
    val red = (255, 0, 0)
    beams.foreach { beam =>
      (beam.x0 to beam.x1).foreach { x =>
        val progress = (x - beam.x0) / (beam.x1 - beam.x0).floatValue
        val y = beam.y0 + ((beam.y1 - beam.y0) * progress).intValue
        demo2(x, y) = red
      }
    }
    demo2.saveTo(new File("demos/beamboxes.%s.png".format(caseName)))
  }

  def targetYFor(x:Int, y:Int, staff:Staff, staffSeparationsMax:Float) = {
    // Equation for y-skewing:
    //   y = midlineYs(x) + staffY * staffSeparations(x)
    // Solve for staffY:
    //   staffY = (y - midlineYs(x)) / staffSeparations(x)
    val midlineY = staff.midlineYs(x)
    val ss = staff.staffSeparations(x)
    if (midlineY > -1 && ss > 0) {
      val staffY = (y - midlineY) / ss * 2.0f

      // We don't want to use staffY for the transformed input, because it
      // has only two pixels per staff line; instead multiply by the max
      // staff separation so no pixels are lost.
      Math.round(staffY/2.0f * staffSeparationsMax).intValue
    }
    else 0
  }

  def targetXFor(sourceX:Int, sourceY:Int, m0:Float, m1:Float, w:Int) = {
    // Equation for x-skewing:
    //   x' = x0 + (m0 + (x0/w)*(m1-m0)) * y
    // Solve for x0 given x':
    //   x' = x0 + m0*y + x0*(m1-m0)*y/w
    //   x' - m0*y = x0 + x0*(m1-m0)*y/w
    //   x0 * (1 + (m1-m0)*y/w) = x' - m0*y
    //   x0 = (x' - m0*y) / (1 + (m1-m0)*y/w)
    Math.round(
      (sourceX - m0 * sourceY) / (1.0f + (m1-m0) * sourceY / w)
    ).intValue
  }

  def demoVerticalSlices(input:GrayImage,
      boxToChildBoxes:Map[BoundingBox,List[BoundingBox]], staffName:String) {
    val demo = input.toColorImage
    val outerColor = (255, 0, 0)
    val innerColor = (128, 0, 0)

    def draw(x:Int, y:Int, color:(Int,Int,Int)) {
      val (r, g, b) = demo(x, y)
      val rNew = (r + color._1) min 255
      val gNew = (g + color._2) min 255
      val bNew = (b + color._3) min 255
      demo(x, y) = (rNew, gNew, bNew)
    }

    def drawBox(box:BoundingBox, color:(Int,Int,Int)) {
      (box.minX to box.maxX).foreach { x =>
        draw(x, box.minY, color)
        draw(x, box.maxY, color)
      }
      (box.minY to box.maxY).foreach { y =>
        draw(box.minX, y, color)
        draw(box.maxX, y, color)
      }
    }

    boxToChildBoxes.keys.foreach { parentBox =>
      val childBoxes = boxToChildBoxes(parentBox)
      childBoxes.foreach { childBox =>
        drawBox(childBox, innerColor)
      }
      drawBox(parentBox, outerColor)
    }

    demo.saveTo(new File("demos/segments.%s.png".format(staffName)))
  }

  def saveWidths(boxes:List[BoundingBox], file:File) = {
    printToFile(file) { writer =>
      writer.println("MinX,MaxX,MinY,MaxY")
      boxes.foreach { box =>
        writer.println("%s,%s,%s,%s".format(
          box.minX, box.maxX, box.minY, box.maxY))
      }
    }
  }

  def powerSet[T](elements:List[T]) : List[Set[T]] = {
    elements match {
      case Nil => List(Set[T]())
      case first :: rest =>
        val recursive = powerSet(rest)
        recursive.map { _ + first } ++ recursive
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

  def findNotesInColumn(box:BoundingBox, orthonormalImage:GrayImage,
      transform:OrthonormalTransform,
      templates:Map[String,GrayImage], templateName:String, threshold:Int,
  finder:(GrayImage,GrayImage,List[(Int,Int,Int)],ColorImage,String)=>List[Int],
      donutDemo:ColorImage,
      caseName:String) : List[TemplateMatch] = {
    val template = templates(templateName)
    val midX = (box.minX + box.maxX) / 2
    val possibleStaffYs = (-8 to 8).toList
    val possiblePoints = possibleStaffYs.map { staffY =>
      (midX, transform.yForStaffY(staffY), staffY)
    }
    val results = finder(orthonormalImage, template,
      possiblePoints, donutDemo, caseName)
    var foundNotes:List[TemplateMatch] = Nil
    if (box.maxX - box.minX + 1 + 4 >= template.w) {
      (0 until results.size).foreach { i =>
        val (centerX, centerY, staffY) = possiblePoints(i)
        val result = results(i)
        if (result >= threshold)
          foundNotes = TemplateMatch(centerX, centerY, template.w, template.h,
            staffY, templateName) :: foundNotes
      }
    }
    foundNotes
  }

  def findImmovableInColumn(box:BoundingBox,
      inputGradientX:GrayImage, inputGradientY:GrayImage,
      templateGradientX:GrayImage, templateGradientY:GrayImage,
      input:GrayImage, template:GrayImage,
      templates:Map[String,GrayImage], templateName:String, threshold:Int,
      fixedStaffY:Int, transform:OrthonormalTransform, caseName:String) :
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
      input, template, possiblePoints, caseName)
    var maxResult = threshold 
    var argmaxNote:Option[TemplateMatch] = None
    (0 until results.size).foreach { i =>
      val (centerX, centerY, staffY) = possiblePoints(i)
      val result = results(i)
      val note = TemplateMatch(centerX, centerY, template.w, template.h,
          staffY, templateName)
//if (templateName == "bass_clef") println(result)
      if (result > maxResult) {
        maxResult = result
        argmaxNote = Some(note)
      }
    }
    argmaxNote
  }

  def matchBoxesToAnnotations(boxes:List[BoundingBox], annotation:Annotation,
      transform:OrthonormalTransform) = {
    var boxToAnnotatedStaffYs = Map[BoundingBox,Set[Int]]()
    var missedPoints = annotation.points
    boxes.foreach { box =>
      val points = annotation.points.filter { point =>
        val (x, y) = transform.transformXY(point.x, point.y)
        val matches = x >= (box.minX - 5) && x <= (box.maxX + 5)
        //if (matches && !missedPoints.contains(point))
        //  throw new RuntimeException(
        //    "Too many bounding boxes for annotation %s".format(point))
        matches
      }
      missedPoints = missedPoints.filter { !points.contains(_) }
      val staffYs = points.map { _.staffY }.toSet
      boxToAnnotatedStaffYs = boxToAnnotatedStaffYs.updated(box, staffYs)
    }
    if (missedPoints.size > 0) {
      //throw new RuntimeException(
      //  "Couldn't find bounding box for annotations: %s".format(missedPoints))
    }
    boxToAnnotatedStaffYs
  }

  def saveGrayImage(image:GrayImage, file:File) {
    image.saveTo(file)
  }

  def loadGrayImage(file:File) : GrayImage = {
    ColorImage.readFromFile(file).toGrayImage
  }

  def saveColorImage(image:ColorImage, file:File) {
    image.saveTo(file)
  }

  def loadColorImage(file:File) : ColorImage = {
    ColorImage.readFromFile(file)
  }

  def saveBounds(bounds:List[BoundingBox], file:File) {
    val boundsAsMaps = bounds.map { _.toMap }
    val out = Json.build(boundsAsMaps).toString().replaceAll(
      "\\},\\{", "},\n{")
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadBounds(file:File) : List[BoundingBox] = {
    val inString = readFile(file)
    Json.parse(inString).asInstanceOf[List[Map[String,Int]]].map { map =>
      BoundingBox(map("minX"), map("maxX"), map("minY"), map("maxY"))
    }
  }

  def saveStaffs(staffs:List[Staff], file:File) {
    val staffAsLists = staffs.map { _.toMap }
    val out = Json.build(staffAsLists).toString().replaceAll(
      "\\},\\{", "},\n{")
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadStaffs(file:File) : List[Staff] = {
    val inString = readFile(file)
    Json.parse(inString).asInstanceOf[List[Map[String,Any]]].map { map =>
      Staff.fromMap(map) 
    }
  }

  def saveBeams(beams:List[Beam], file:File) {
    val out = Json.build(beams.map { _.toMap }).toString().replaceAll(
      "\\},\\{", "},\n{")
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadBeams(file:File) : List[Beam] = {
    val inString = readFile(file)
    Json.parse(inString).asInstanceOf[List[Map[String,Int]]].map { map =>
      Beam.fromMap(map)
    }
  }

  def saveFloatPair(floats:(Float,Float), file:File) {
    printToFile(file) { writer =>
      writer.write(Json.build(List(floats._1, floats._2)).toString())
    }
  }

  def loadFloatPair(file:File) : (Float,Float) = {
    val inString = readFile(file)
    val List(int1, int2) = Json.parse(inString).asInstanceOf[List[BigDecimal]]
    (int1.toFloat, int2.toFloat)
  }

  def saveVLines(vLines:List[VLine], file:File) {
    val vLinesAsMaps = vLines.map { _.toMap }
    val out = Json.build(vLinesAsMaps).toString().replaceAll(
      "\\},\\{", "},\n{")
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadVLines(file:File) : List[VLine] = {
    val inString = readFile(file)
    Json.parse(inString).asInstanceOf[List[Map[String,Int]]].map {
      VLine.fromMap(_) }
  }

  def saveOrthonormalTransform(transform:OrthonormalTransform, file:File) {
    val out = Json.build(transform.toMap).toString()
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadOrthonormalTransform(file:File) : OrthonormalTransform = {
    val inString = readFile(file)
    val map = Json.parse(inString).asInstanceOf[Map[String,Any]]
    OrthonormalTransform.fromMap(map)
  }

  def saveBoxesMap(map:Map[BoundingBox,List[BoundingBox]], file:File) = {
    val pairs = map.map { pair => List(pair._1.toMap, pair._2.map { _.toMap }) }
    val out = Json.build(pairs).toString()
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadBoxesMap(file:File) : Map[BoundingBox,List[BoundingBox]] = {
    val inString = readFile(file)
    val pairs = Json.parse(inString).asInstanceOf[List[List[Any]]]
    pairs.map { pair =>
      (BoundingBox.fromMap(pair(0).asInstanceOf[Map[String,Int]]),
      pair(1).asInstanceOf[List[Map[String,Int]]].map { map =>
        BoundingBox.fromMap(map.asInstanceOf[Map[String,Int]])
      })
    }.toMap
  }

  def processCase(caseName:String) : Performance = {
    var casePerformance = Performance(Set(), Set(), Set())

    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage
    val annotationPath = "input/%s.json".format(caseName)
    val annotationString = scala.io.Source.fromFile(annotationPath).mkString
    val annotation = loadAnnotationJson(annotationString)

    val midlinesPath = new File("output/midlines/%s.jpeg".format(caseName))
    val midlines = readOrGenerate(midlinesPath, saveColorImage, loadColorImage){
      () => FindMidlines.run(image, caseName)
    }

    val boundsPath = new File("output/bounds/%s.json".format(caseName))
    val bounds = readOrGenerate(boundsPath, saveBounds, loadBounds) { () =>
      FindBounds.run(midlines, caseName)
    }

    val staffsPath = new File("output/staffs/%s.json".format(caseName))
    val staffs = readOrGenerate(staffsPath, saveStaffs, loadStaffs) { () =>
      FindStaffs.run(midlines, image, bounds, caseName)
    }

    var filteredNotes = List[Set[TemplateMatch]]()
    staffs.foreach { staffAbsolute =>
      val staffName = staffAbsolute.staffName
      val (x0, x1) = (staffAbsolute.bounds.minX, staffAbsolute.bounds.maxX)
      val y0 = (Math.floor(staffAbsolute.midlineYs.filter { _ > -1 }.min -
        staffAbsolute.staffSeparations.max * 8.0f).intValue) max 0
      val y1 = (Math.ceil(staffAbsolute.midlineYs.filter { _ > -1 }.max +
        staffAbsolute.staffSeparations.max * 8.0f).intValue) min (image.h - 1)
      val (w, h) = (x1 - x0 + 1, y1 - y0 + 1)

      val midlineYs = new Array[Int](w)
      (0 until w).foreach { x =>
        val oldY = staffAbsolute.midlineYs(x + x0)
        midlineYs(x) = (if (oldY == -1) oldY else oldY - y0)
      }

      val staffSeparations = new Array[Float](w)
      (0 until w).foreach { x =>
        staffSeparations(x) = staffAbsolute.staffSeparations(x + x0)
      }

      val staffRelative = Staff(staffName,
        BoundingBox(0, x1 - x0, 0, y1 - y0), midlineYs, staffSeparations)
      val cropped = image.crop(x0, y0, w, h)

      val erasedPath1 = new File("output/erased1/%s.jpeg".format(staffName))
      val justNotes =
          readOrGenerate(erasedPath1, saveGrayImage, loadGrayImage) { () =>
        EraseStaff.run(cropped, staffRelative, true, staffName)
      }
      
      val erasedPath2 = new File("output/erased2/%s.jpeg".format(staffName))
      val justNotes2 =
          readOrGenerate(erasedPath2, saveGrayImage, loadGrayImage) { () =>
        EraseStaff.run(cropped, staffRelative, false, staffName)
      }

      val beamsPath = new File("output/beams/%s.json".format(staffName))
      val beams = readOrGenerate(beamsPath, saveBeams, loadBeams) { () =>
        FindBeams.run(justNotes, cropped, staffRelative, staffName)
      }

      val noBeamsPath = new File("output/no_beams/%s.jpeg".format(staffName))
      val justNotesNoBeams =
          readOrGenerate(noBeamsPath, saveGrayImage, loadGrayImage) { () =>
        EraseBeams.run(justNotes2, beams, staffRelative, staffName)
      }

      val vSlopeRangePath =
        new File("output/v_slope_range/%s.json".format(staffName))
      val vSlopeRange = readOrGenerate(vSlopeRangePath,
          saveFloatPair, loadFloatPair) { () =>
        FindVSlopeRange.run(justNotes2, cropped, staffName)
      }

      val vLinesPath = new File("output/v_lines/%s.json".format(staffName))
      val vLines = readOrGenerate(vLinesPath, saveVLines, loadVLines) { () =>
        FindVLines.run(justNotes2, cropped, vSlopeRange, staffName)
      }

      val transformPath = new File(
        "output/orthonormal_transform/%s.json".format(staffName))
      val transform = readOrGenerate(transformPath, saveOrthonormalTransform,
          loadOrthonormalTransform) { () =>
        SetupOrthonormalTransform.run(cropped.w, cropped.h, staffRelative,
          vSlopeRange, staffName)
      }

      val orthonormalImagePath = new File(
        "output/orthonormal_image/%s.png".format(staffName))
      val orthonormalImage = readOrGenerate(
          orthonormalImagePath, saveGrayImage, loadGrayImage) { () =>
        DoOrthonormalTransform.run(justNotesNoBeams, transform)
      }

      val slicesPath = new File("output/slices/%s.json".format(staffName))
      val boxToChildBoxes = readOrGenerate(
          slicesPath, saveBoxesMap, loadBoxesMap) { () =>
        FindVerticalSlices.run(orthonormalImage, transform, vLines, staffName)
      }
      val allChildBoxes =
        boxToChildBoxes.values.foldLeft(List[BoundingBox]()){ _++_ }
      demoVerticalSlices(orthonormalImage, boxToChildBoxes, staffName)
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

    var predictedNotes:List[Set[TemplateMatch]] = Nil
    val boxToAnnotatedStaffYs = matchBoxesToAnnotations(
      boxToChildBoxes.keys.toList, annotation, transform)
    val donutDemo = orthonormalImage.toColorImage
    boxToChildBoxes.keys.toList.sortBy { _.minX }.foreach { parentBox =>
      printf("  Box at x=%04d,    ".format(parentBox.minX))
      var foundNotes:Set[TemplateMatch] = 
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          templateTrebleGradientX, templateTrebleGradientY,
          orthonormalImage, templates("treble_clef"),
          templates, "treble_clef", 10000, gClefStaffY, transform, caseName
          ).toSet ++
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          templateBassGradientX, templateBassGradientY,
          orthonormalImage, templates("bass_clef"),
          templates, "bass_clef", 4000, fClefStaffY, transform, caseName
          ).toSet ++
        findImmovableInColumn(parentBox, inputGradientX, inputGradientY,
          template44GradientX, template44GradientY,
          orthonormalImage, templates("44"),
          templates, "44", 3000, middleStaffY, transform, caseName).toSet
      if (foundNotes.size == 0) {
        val childBoxes = boxToChildBoxes(parentBox)
        childBoxes.foreach { box =>
          val newFoundNotes =
            findNotesInColumn(box, orthonormalImage, transform, templates,
              "white_head", 10, findWhiteHeads, donutDemo, caseName).toSet ++
            findNotesInColumn(box, orthonormalImage, transform, templates,
              "black_head", 20, findBlackHeads, donutDemo, caseName).toSet
          if (newFoundNotes.size > 0)
            foundNotes ++= chooseBestOverlappingSets(
              parentBox, newFoundNotes, templates, orthonormalImage)
        }
      }
      predictedNotes ++= List(foundNotes)
    }
    //donutDemo.saveTo(new File("demos/donut_demo.%s.png".format(caseName)))
    filteredNotes ++= predictedNotes

//demoNotes(predictedNotes, orthonormalImage.toColorImage, staffName)
    val demo = orthonormalImage.toColorImage
    predictedNotes.foldLeft(Set[TemplateMatch]()) { _ ++ _ }.foreach { point =>
      val color =
        /*if (performance.correctNotes.filter{ _._2 == point }.size > 0)
          (0, 128, 0)
        else*/
          (255, 0, 0) // spurious
        drawTemplateMatch(point, demo, templates(point.templateName), color)
    }
    demo.saveTo(new File("demos/notes.%s.png".format(staffName)))


/*
    val c = metrics.cSpacing.intValue
    val templateSpecs =
      //TemplateSpec("treble_clef",   3,    8, findTrebleClef, 0.1) ::
      //TemplateSpec("sharp",       1.3,  2.6, findAccidental) ::
      //TemplateSpec("flat",        1.1, 2.35, findAccidental) ::
      //TemplateSpec("natural",       1,    3, findAccidental) ::
      TemplateSpec("black_head",   2.00, 1.25, findBlackHeads, 0.25) ::
      //TemplateSpec("white_head",  1.5, 1.25, findWhiteHeads, 1.0) ::
      Nil
    var casePerformance = Performance(List(), List(), List())
    var points:List[TemplateMatch] = Nil
    var templates = Map[String,GrayImage]()
    templateSpecs.foreach { templateSpec =>
      val templateName = templateSpec.name
      printf(" Searching for %s...\n", templateName)

      val templatePath = new File("templates/%s.png".format(templateName))
      val bigTemplate =
        ColorImage.readFromFile(templatePath).toGrayImage.inverse
      val templateW =
        (templateSpec.widthInStaffLines * metrics.cSpacing).intValue
      val templateH =
        (templateSpec.heightInStaffLines * metrics.cSpacing).intValue
      val template = scaleTemplate(bigTemplate, templateW, templateH)
      templates = templates.updated(templateName, template)
      val detected = templateSpec.finder(
        justNotes, template, templateName + "." + caseName)
      detected.saveTo(new File("demos/detected.%s.%s.png".format(
        templateName, caseName)))

      val thresholdInt =
        (templateSpec.threshold * templateW * templateH).intValue
      val detectedThreshold =
        ColorImage.giveRGBPerPixel(detected.w, detected.h) { (x, y) =>
          val v = detected(x, y)
          if (v < thresholdInt) (v * 255 / thresholdInt, 0, 0)
          else (v, v, v)
        }
      detectedThreshold.saveTo(new File("demos/detected2.%s.%s.png".format(
        templateName, caseName)))
      points ++= gleanPoints(detected, metrics, yCorrection, templateW,
        templateH, thresholdInt, templateName)
    }

    val overlappingPointGroups = groupOverlappingPoints(points)
    demoAlternatives(overlappingPointGroups, inputAdjusted, caseName)
    demoPointGroups(overlappingPointGroups, inputAdjusted, caseName)
    val culledPointGroups = chooseBestOverlappingSets(
      overlappingPointGroups, templates, justNotes.inverse)

    val groupedPoints = groupTemplateMatches(culledPointGroups)
    val filteredNotes = groupedPoints

*/
} // next staff

    println("  calcPerformance")
    val performance = calcPerformance(filteredNotes, annotation.notes)
    println("Case %2s: precision: %.3f, recall: %.3f".format(
      caseName, performance.precision, performance.recall))
    //printf("   correct: %s\n", performance.correctNotes.toString.replaceAll(
    //    "\\)\\), \\(", ")),\n                ("))
    if (performance.spuriousNotes.size > 0)
      printf("  spurious: %s\n", performance.spuriousNotes.toString.replaceAll(
        "\\)\\), \\(", ")),\n                ("))
    if (performance.missingNotes.size > 0)
      printf("   missing: %s\n", performance.missingNotes.toString.replaceAll(
        "\\)\\), \\(", ")),\n                ("))

    /*val demo = orthonormalImage.toColorImage
    filteredNotes.foldLeft(Set[TemplateMatch]()) { _ ++ _ }.foreach { point =>
      val color =
        if (performance.correctNotes.filter{ _._2 == point }.size > 0)
          (0, 128, 0)
        else
          (255, 0, 0) // spurious
        drawTemplateMatch(point, demo, templates(point.templateName), color)
    }
    demo.saveTo(new File("demos/notes.%s.png".format(caseName)))*/

    casePerformance = Performance(
      casePerformance.correctNotes ++ performance.correctNotes,
      casePerformance.spuriousNotes ++ performance.spuriousNotes,
      casePerformance.missingNotes ++ performance.missingNotes)
    casePerformance
  }

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

  def main(args:Array[String]) {
    try {
      println(Colors.ansiEscapeToHighlightProgramOutput)

      var globalPerformance = Performance(Set(), Set(), Set())
      val caseNames = expandCaseNames(args)
      caseNames.foreach { caseName =>
        println("Case %s:".format(caseName))
        val performance = processCase(caseName)
        globalPerformance = Performance(
          globalPerformance.correctNotes ++ performance.correctNotes,
          globalPerformance.spuriousNotes ++ performance.spuriousNotes,
          globalPerformance.missingNotes ++ performance.missingNotes)
      }
      println("Total:   precision: %.3f -- recall: %.3f".format(
        globalPerformance.precision, globalPerformance.recall))

    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      println(Colors.ansiEscapeNormal)
    }
    System.exit(0)
  }
}
