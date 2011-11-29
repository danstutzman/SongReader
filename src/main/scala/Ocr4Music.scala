import com.twitter.json.Json
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.SysexMessage
import scala.collection.mutable.{Map => MutableMap}
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

case class LabeledPoint (
  val label:String,
  val templateName:String,
  val x:Int,
  val y:Int,
  val staffY:Int
) {}

case class AnnotationBox (
  val num:Int,
  val left:Int,
  val top:Int,
  val width:Int,
  val height:Int,
  val points:List[LabeledPoint],
  val pointGroups:List[Set[LabeledPoint]]
) {}

case class Performance (
  val correctNotes:Set[(Int,TemplateMatch)],
  val spuriousNotes:Set[(Int,TemplateMatch)],
  val missingNotes:Set[(Int,LabeledPoint)]
) {
  def numCorrect() = { correctNotes.size }
  def numSpurious() = { spuriousNotes.size }
  def numMissing() = { missingNotes.size }
  def precision() = { numCorrect.floatValue / (numCorrect + numSpurious) }
  def recall() = { numCorrect.floatValue / (numCorrect + numMissing) }
  def +(other:Performance) = { Performance(
    correctNotes ++ other.correctNotes,
    spuriousNotes ++ other.spuriousNotes,
    missingNotes ++ other.missingNotes)
  }
}

case class TemplateSpec (
  val name:String,
  val widthInStaffLines:Double,
  val heightInStaffLines:Double,
  val finder:(GrayImage,GrayImage,String)=>GrayImage,
  val threshold:Double
) {}
 
class TemplateResizer(val cache:MutableMap[(String,Int,Int),GrayImage]) {
  def this() = this(MutableMap[(String,Int,Int),GrayImage]())
  def apply(point:TemplateMatch) = {
    val key = (point.templateName, point.w, point.h)
    if (cache.isDefinedAt(key)) cache(key)
    else cache.getOrElseUpdate(key,
      FillBoxes.prepareTemplate(point.templateName, point.w, point.h))
  }
}

object Ocr4Music {
  def loadAnnotationsJson(annotationString : String) : List[AnnotationBox] = {
    val annotationsJson = Json.parse(annotationString)
    val boxes = annotationsJson.asInstanceOf[List[Map[String,Any]]].map { box =>
      val points = box("points").asInstanceOf[List[Map[String,Any]]].flatMap {
          point =>
        val maybeTemplateName = point("type") match {
          case "F"  => Some("bass_clef")
          case "G"  => Some("treble_clef")
          case "44" => Some("44")
          case "8"  => Some("black_head")
          case "4"  => Some("black_head")
          case "2"  => Some("white_head")
          case _    => None
        }
        maybeTemplateName.map { templateName =>
          LabeledPoint(
            point("type").asInstanceOf[String],
            templateName,
            point("x").asInstanceOf[Int],
            point("y").asInstanceOf[Int],
            point("staffY").asInstanceOf[Int]
          )
        }
      }

      var noteGroups:List[Set[LabeledPoint]] = Nil
      var currentNoteGroup = Set[LabeledPoint]()
      var lastNoteX = -999
      points.sortBy { _.x }.foreach { point =>
        if (Math.abs(point.x - lastNoteX) >= 20 && currentNoteGroup.size > 0) {
          noteGroups = currentNoteGroup :: noteGroups
          currentNoteGroup = Set[LabeledPoint]()
        }
        currentNoteGroup += point
        lastNoteX = point.x
      }
      if (currentNoteGroup.size > 0)
        noteGroups = currentNoteGroup :: noteGroups
      val notes = noteGroups.reverse
  
      AnnotationBox(
        box("num").asInstanceOf[Int],
        box("left").asInstanceOf[Int],
        box("top").asInstanceOf[Int],
        box("width").asInstanceOf[Int],
        box("height").asInstanceOf[Int],
        points,
        notes
      )
    }
    boxes
  }

  // Levenshtein minimum-edit-distance to determine best alignment
  // between detected note groups and ground truth note groups.
  // This ensures that a spurious or missed note only causes one
  // error instead of throwing off all the notes to the right.
  def calcPerformance(
      estimated:List[Set[TemplateMatch]],
      annotated:List[Set[LabeledPoint]]) = {
    //printf("init estimated: %s\n", estimated.map { _.map { _.staffY } })
    //printf("init annotated: %s\n", annotated.map { _.map { _.staffY } })

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
          estimated(x - 1).foreach { note =>
            if (!(annotated(y - 1).exists { note2 =>
              note2.templateName == note.templateName &&
              note2.staffY == note.staffY
            })) {
              scoreIncrease += 1
            }
          }
          annotated(y - 1).foreach { note =>
            if (!(estimated(x - 1).exists { note2 =>
              note2.templateName == note.templateName &&
              note2.staffY == note.staffY
            })) {
              scoreIncrease += 1
            }
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
      val annotated:Set[LabeledPoint]
    ) {}
    var pairedNoteGroups:List[PairedNoteGroup] = Nil
    var x = w
    var y = h
    while (x > 0 || y > 0) {
      backPointer(y)(x) match {
        case (-1, 0) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimated(x - 1), Set()) ::
            pairedNoteGroups
          x -= 1
        case (0, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, Set(), annotated(y - 1)) ::
            pairedNoteGroups
          y -= 1
        case (-1, -1) =>
          pairedNoteGroups =
            PairedNoteGroup(x - 1, estimated(x - 1), annotated(y - 1)) ::
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
    var missingNotes:List[(Int,LabeledPoint)] = Nil // staffX, (type, staffY)
    pairedNoteGroups.foreach { pair =>
      val PairedNoteGroup(staffX, estimatedNotes, annotated) = pair
      estimatedNotes.foreach { note =>
        if (annotated.exists { note2 =>
          note2.templateName == note.templateName &&
          note2.staffY       == note.staffY
        }) {
          correctNotes = (staffX, note) :: correctNotes
        } else {
          spuriousNotes = (staffX, note) :: spuriousNotes
        }
      }
      annotated.foreach { note =>
        if (!(estimatedNotes.exists { note2 =>
          note2.templateName == note.templateName &&
          note2.staffY       == note.staffY
        })) {
          missingNotes = (staffX, note) :: missingNotes
        }
      }
    }

    Performance(correctNotes.toSet, spuriousNotes.toSet, missingNotes.toSet)
  }

  def drawTemplateMatch(
      _match:TemplateMatch, output:ColorImage, template:GrayImage,
      rgb:(Int,Int,Int)) {
    val templateScaled = FillBoxes.scaleTemplate(template, _match.w, _match.h)
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

  def chooseBestOverlappingSets(
      bounds:BoundingBox,
      overlappingPointGroup:Set[TemplateMatch],
      templateResizer:TemplateResizer, orthonormalImage:GrayImage) :
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
        val template = templateResizer(point)
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
      boxToChildBoxes:Map[BoundingBox,List[BoundingBox]], staffName:String) = {
    val demo = input.toColorImage
    val outerColor = (0, 0, 255)
    val innerColor = (0, 0, 128)

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
    demo
  }

  def matchBoxesToAnnotations(boxes:List[BoundingBox],
      annotationPoints:List[LabeledPoint], transform:OrthonormalTransform) = {
    var boxToAnnotatedStaffYs = Map[BoundingBox,Set[Int]]()
    var missedPoints = annotationPoints
    boxes.foreach { box =>
      val points = annotationPoints.filter { point =>
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

  def saveFilledBoxes(boxes:List[BoxOfTemplates], file:File) = {
    val out = Json.build(boxes.map { _.toMap }).toString()
    printToFile(file) { writer =>
      writer.write(out)
    }
  }

  def loadFilledBoxes(file:File) : List[BoxOfTemplates] = {
    val inString = readFile(file)
    val maps = Json.parse(inString).asInstanceOf[List[Map[String,Any]]]
    maps.map { map => BoxOfTemplates.fromMap(map) }
  }

  def saveCaseStudy(_match:TemplateMatch, orthonormalImage:GrayImage,
      templateResizer:TemplateResizer, judgment:String, staffName:String) {

    val dilatedOrthonormal =
      new GrayImage(orthonormalImage.w, orthonormalImage.h)
    val erodedOrthonormal =
      new GrayImage(orthonormalImage.w, orthonormalImage.h)
    (0 until orthonormalImage.w).foreach { x =>
      (0 until orthonormalImage.h).foreach { y =>
        var minV = orthonormalImage(x, y)
        var maxV = orthonormalImage(x, y)
        (-1 to 1).foreach { neighborX =>
          (-1 to 1).foreach { neighborY =>
            val v = orthonormalImage(x + neighborX, y + neighborY)
            if (v > maxV) {
              maxV = v
            }
            if (v < minV) {
              minV = v
            }
          }
        }
        dilatedOrthonormal(x, y) = maxV
        erodedOrthonormal(x, y) = minV
      }
    }

    val input = new GrayImage(_match.w, _match.h)
    val template = templateResizer(_match)
    val blackInput = new GrayImage(_match.w, _match.h)
    val blackTemplate = new GrayImage(_match.w, _match.h)
    val erodedInput = new GrayImage(_match.w, _match.h)
    val blackMatch = new ColorImage(_match.w, _match.h)
    val blackMatch2 = new ColorImage(_match.w, _match.h)
    val dilatedInput = new GrayImage(_match.w, _match.h)
    val whiteMatch = new ColorImage(_match.w, _match.h)
    val whiteMatch2 = new ColorImage(_match.w, _match.h)
    val combinedMatch = new GrayImage(_match.w, _match.h)
    val combinedMatch2 = new GrayImage(_match.w, _match.h)
    var combinedMatch2Score = 0
    (0 until input.w).foreach { x =>
      (0 until input.h).foreach { y =>
        val inputV = orthonormalImage(_match.x - _match.w/2 + x,
                                      _match.y - _match.h/2 + y)
        val dilatedV = dilatedOrthonormal(_match.x - _match.w/2 + x,
                                          _match.y - _match.h/2 + y)
        val erodedV = erodedOrthonormal(_match.x - _match.w/2 + x,
                                        _match.y - _match.h/2 + y)
        val templateV = template(x, y)
        val inputIsBlack = inputV >= 200
        val templateIsBlack = templateV >= 200
        val dilatedIsBlack = dilatedV >= 200

        val templateIsWhite = templateV <= 200
        val inputIsWhite = inputV <= 200
        val erodedIsWhite = erodedV <= 200

        input(x, y) = inputV
        blackInput(x, y) = (if (inputIsBlack) 255 else 0)
        blackTemplate(x, y) = (if (templateIsBlack) 255 else 0)
        dilatedInput(x, y) = (if (dilatedIsBlack) 255 else 0)
        blackMatch(x, y) = (if (templateIsBlack && !inputIsBlack) 255 else 0,
                            if (templateIsBlack && inputIsBlack) 255 else 0,
                            if (!templateIsBlack && inputIsBlack) 255 else 0)
        blackMatch2(x, y) = (if (templateIsBlack && !dilatedIsBlack) 255 else 0,
                             if (templateIsBlack && dilatedIsBlack) 255 else 0,
                             if (!templateIsBlack && dilatedIsBlack) 255 else 0)
        erodedInput(x, y) = (if (!erodedIsWhite) 255 else 0)
        whiteMatch(x, y) = (if (templateIsWhite && !inputIsWhite) 255 else 0,
                             if (templateIsWhite && inputIsWhite) 255 else 0,
                             if (!templateIsWhite && inputIsWhite) 255 else 0)
        whiteMatch2(x, y) = (if (templateIsWhite && !erodedIsWhite) 255 else 0,
                             if (templateIsWhite && erodedIsWhite) 255 else 0,
                             if (!templateIsWhite && erodedIsWhite) 255 else 0)
        combinedMatch(x, y) = if ((templateIsBlack && inputIsBlack) ||
                                   templateIsWhite && inputIsWhite) 255 else 0
        combinedMatch2(x, y) = if ((templateIsBlack && dilatedIsBlack) ||
                                    templateIsWhite && erodedIsWhite) 255 else 0
        combinedMatch2Score += (combinedMatch2(x, y) / 255)
      }
    }

    val images:List[ColorImage] = List(
      input.toColorImage,
      template.toColorImage,
      blackInput.toColorImage,
      blackTemplate.toColorImage,
      erodedInput.toColorImage,
      blackMatch,
      blackMatch2,
      dilatedInput.toColorImage,
      whiteMatch,
      whiteMatch2,
      combinedMatch.toColorImage,
      combinedMatch2.toColorImage
    )
    val bigImage = new ColorImage(_match.w * images.size, _match.h)
    images.zipWithIndex.foreach { pair =>
      val (image, i) = pair
      (0 until _match.w).foreach { x =>
        (0 until _match.h).foreach { y =>
          bigImage(i * _match.w + x, y) = image(x, y)
        }
      }
    }
    val score = combinedMatch2Score * 100 / (_match.w * _match.h)
    bigImage.saveTo(new File(
      "output/case_studies/%s/%02d.%s.%s.%03d.%03d.png".format(
      _match.templateName,
      score, judgment, staffName, _match.x, _match.y)))
  }

  def allTemplatesFromBoxes(boxes:List[BoxOfTemplates]) = {
    boxes.foldLeft(Set[TemplateMatch]()) { (accum, box) =>
      if (box.templates.size > 0)
        accum ++ box.templates
      else
        accum ++ box.childBoxes.foldLeft(Set[TemplateMatch]()) {
           (accum2, childBox) => accum2 ++ childBox.templates
        }
    }
  }

  def chooseBestNotes(boxes:List[BoxOfTemplates], orthonormalImage:GrayImage) :
      List[Set[TemplateMatch]] ={
    boxes.map { unfilteredBox =>
      val box = BoxOfTemplates(unfilteredBox.box,
        unfilteredBox.templates.filter { template =>
          val threshold = template.templateName match {
            case "treble_clef" => 4000
            case "bass_clef"   => 4000
            case "44"          => 3000
          }
          template.score >= threshold
        },
        unfilteredBox.childBoxes.map { unfilteredBox2 =>
          BoxOfTemplates(unfilteredBox2.box,
            unfilteredBox2.templates.filter { template =>
              val threshold = template.templateName match {
                case "black_head" => 20
                case "white_head" => 10
              }
              template.score >= threshold
            },
            List[BoxOfTemplates]() // assume no recursion
          )
        }
      )
      if (box.templates.size > 0)
        box.templates
      else
        box.childBoxes.foldLeft(Set[TemplateMatch]()) { (accum, childBox) =>
          accum ++ chooseBestOverlappingSets(childBox.box, childBox.templates,
            new TemplateResizer(), orthonormalImage)
        }
    }
  }

  def outputPerformance(performance:Performance, staffName:String) {
    println("Staff %2s: precision: %.3f, recall: %.3f".format(
      staffName, performance.precision, performance.recall))
    printf("   correct: %s\n", performance.correctNotes.toString.replaceAll(
        "\\)\\), \\(", ")),\n                ("))
    if (performance.spuriousNotes.size > 0)
      printf("  spurious: %s\n", performance.spuriousNotes.toString.
        replaceAll("\\)\\), \\(", ")),\n                ("))
    if (performance.missingNotes.size > 0)
      printf("   missing: %s\n", performance.missingNotes.toString.replaceAll(
        "\\)\\), \\(", ")),\n                ("))
  }

  def demoPredictedNotes(predictedNotes:List[Set[TemplateMatch]],
      consideredNotes:Set[TemplateMatch], orthonormalImage:GrayImage,
      performance:Performance, bounds:BoundingBox,
      transform:OrthonormalTransform, demo:ColorImage, staffName:String) {
    val templateResizer = new TemplateResizer()
    performance.missingNotes.foreach { xAndPoint =>
      val (_, point) = xAndPoint
      var maybeClosestMatch:Option[TemplateMatch] = None
      var minDistance = 40 // can't be further away than this
      if (point.x >= bounds.minX && point.x <= bounds.maxX &&
          point.y >= bounds.minY && point.y <= bounds.maxY) {
        var (orthoX, orthoY) =
          transform.transformXY(point.x - bounds.minX, point.y - bounds.minY)
        consideredNotes.foreach { _match =>
          if (_match.staffY == point.staffY &&
              _match.templateName == point.templateName) {
            val distance = Math.abs(_match.x - orthoX) +
                           Math.abs(_match.y - orthoY)
            if (distance < minDistance) {
              minDistance = distance
              maybeClosestMatch = Some(_match)
            }
          }
        }
      }
      maybeClosestMatch.foreach { _match =>
        val color = (255, 255, 0) // yellow means missed
        drawTemplateMatch(_match, demo, templateResizer(_match), color)
        saveCaseStudy(_match, orthonormalImage, templateResizer, "missed",
          staffName)
      }
    }
    predictedNotes.foreach { pointGroup =>
      pointGroup.foreach { point =>
        val isCorrect =
          (performance.correctNotes.filter{ _._2 == point }.size > 0)
        val color = if (isCorrect) (0, 128, 0) else (255, 0, 0)
        drawTemplateMatch(point, demo, templateResizer(point), color)
        saveCaseStudy(point, orthonormalImage, templateResizer,
          (if (isCorrect) "correct" else "spurious"), staffName)
      }
    }
    demo.saveTo(new File("demos/notes.%s.png".format(staffName)))
  }

  def writeToMidi(predictedNotes:List[Set[TemplateMatch]], caseName:String) {
    // create a new MIDI sequence with 24 ticks per beat
    val sequence = new Sequence(javax.sound.midi.Sequence.PPQ, 24)
    val track = sequence.createTrack()
    var currentTime = 0L

    def wait(numTicks:Int) {
      currentTime += numTicks
    }
    def add(message:MidiMessage) {
      track.add(new MidiEvent(message, currentTime))
    }

    // turn on General MIDI sound set
    val sm = new SysexMessage()
    sm.setMessage(Array[Byte](0xF0.asInstanceOf[Byte], 0x7E, 0x7F,
                  0x09, 0x01, 0xF7.asInstanceOf[Byte]), 6)
    add(sm)

    // set tempo
    val microsecondsPerQuarter = (0.6 * 1000 * 1000).intValue
    val mt = new MetaMessage()
    mt.setMessage(0x51, Array[Byte](
      (microsecondsPerQuarter >> 16).asInstanceOf[Byte],
      (microsecondsPerQuarter >> 8 % 256).asInstanceOf[Byte],
      (microsecondsPerQuarter % 256).asInstanceOf[Byte]), 3)
    add(mt)

    // set track name
    val trackName = caseName
    val mt2 = new MetaMessage()
    mt2.setMessage(0x03, trackName.getBytes(), trackName.length())
    add(mt2)

    // set omni on
    val mm = new ShortMessage()
    mm.setMessage(ShortMessage.CONTROL_CHANGE, 0x7D, 0x00)
    add(mm)

    // set poly on
    val mm2 = new ShortMessage()
    mm2.setMessage(ShortMessage.CONTROL_CHANGE, 0x7F, 0x00)
    add(mm2)

    // set instrument to piano
    val mm3 = new ShortMessage()
    mm3.setMessage(ShortMessage.PROGRAM_CHANGE, 0x00, 0x00)
    add(mm3)

    val middleC = 60
    val bAboveMiddleC = middleC + 11
    val dBelowMiddleC = middleC - 10
    var isBassClef = false
    predictedNotes.foreach { concurrentNotes =>
      val durationInTicks = (List(0) ++ concurrentNotes.map {
        _.templateName match {
          case "white_head" => 48
          case "black_head" => 24
          case _ => 0
        }
      }).max

      concurrentNotes.foreach {
        _.templateName match {
          case "treble_clef" => isBassClef = false
          case "bass_clef"   => isBassClef = true
          case _             => ()
        }
      }

      val noteNums = concurrentNotes.map { note =>
        if (!isBassClef) {
          val halfStepsAboveB = note.staffY match {
            case -8 =>  13
            case -7 =>  12
            case -6 =>  10
            case -5 =>   8
            case -4 =>   6
            case -3 =>   5
            case -2 =>   3
            case -1 =>   1
            case  0 =>   0
            case  1 =>  -2
            case  2 =>  -4
            case  3 =>  -6
            case  4 =>  -7
            case  5 =>  -9
            case  6 => -11
            case  7 => -12
            case  8 => -14
            case _  => 0
          }
          bAboveMiddleC + halfStepsAboveB
        } else { // if it is bass clef
          val halfStepsAboveD = note.staffY match {
            case -8 =>  14
            case -7 =>  12
            case -6 =>  10
            case -5 =>   9
            case -4 =>   7
            case -3 =>   5
            case -2 =>   3
            case -1 =>   2
            case  0 =>   0
            case  1 =>  -2
            case  2 =>  -3
            case  3 =>  -5
            case  4 =>  -6
            case  5 =>  -8
            case  6 =>  -9
            case  7 => -11
            case  8 => -13
            case _  => 0
          }
          dBelowMiddleC + halfStepsAboveD
        }
      }

      if (durationInTicks > 0) {
        noteNums.foreach { noteNum =>
          val mm4 = new ShortMessage()
          mm4.setMessage(ShortMessage.NOTE_ON, noteNum, 0x60)
          add(mm4)
        }
        wait(durationInTicks)
        noteNums.foreach { noteNum =>
          val mm5 = new ShortMessage();
          mm5.setMessage(ShortMessage.NOTE_OFF, noteNum, 0x40);
          add(mm5)
        }
      }
    }

    // set end of track (meta event)
    val mt3 = new MetaMessage();
    mt3.setMessage(0x2F, Array[Byte](), 0);
    wait(24)
    add(mt3)

    val path = new File("output/midi/%s.mid".format(caseName))
    MidiSystem.write(sequence, 1, path)
  }

  def highlightMinimums(image:GrayImage) : GrayImage = {
    val out = new GrayImage(image.w, image.h)
    val size = 1
    (0 until image.w).foreach { x =>
      (0 until image.h).foreach { y =>
        var max = 0
        var min = 255
        (-size to size).foreach { yDelta =>
          val v = image(x, y + yDelta)
          if (v > max) max = v
          if (v < min) min = v
        }
        val isMin = (image(x, y) == min)

        if (isMin) {
          val v = max - min
          out(x, y) = v
        } else {
          out(x, y) = 0
        }
      }
    }
    out
  }

  def detectStaffs(image:GrayImage, caseName:String) :
      (Array[Array[Int]], Array[Array[Int]]) = {
    val demo = highlightMinimums(image)
    val thresholdX = 20
    val thresholdY = 5

    case class Point(val x:Int, val centerY:Int, val ys:List[Int],
      val ySpan:Int, threshold:Int, stackHeight:Int) {}

    case class Group(val points:List[Point], box:BoundingBox) {}

    def gatherPoints(threshold:Int, stackHeight:Int) : List[Point] = {
      var points = List[Point]()
      (0 until image.w).foreach { x =>
        var numBlacks = 0
        var vBeforeBlacks = 0
        var yBeforeBlacks = 0
        var numLinks = 0
        var ys = List[Int]()
        var staffBegin = 0
        (0 until image.h).foreach { y =>
          val v = demo(x, y)
          if (v <= 0) { // less than so it can detect blue (under zero) too
            numBlacks += 1
          } else {
            if ((numBlacks >= 2 && numBlacks <= 6) &&
                vBeforeBlacks >= threshold && v >= threshold) {
              numLinks += 1
              if (ys.size == 0)
                ys = List(yBeforeBlacks)
              ys = y :: ys
              if (numLinks >= stackHeight) {
                val centerY = (staffBegin + y) / 2
                points = Point(x, centerY, ys, y - staffBegin,
                  threshold, stackHeight) :: points
              }
            } else {
              numLinks = 0
              ys = List[Int]()
              staffBegin = y
            }
            vBeforeBlacks = v
            yBeforeBlacks = y
            numBlacks = 0
          }
        }
      }
      points
    }

    def expandExistingGroup(
        originalGroups:List[Group], allPoints:List[Point]) = {
      var expandedGroups = originalGroups.toArray
      allPoints.foreach { point =>
        var minMinDistance = 999999
        var argminGroupNum:Option[Int] = None
        originalGroups.zipWithIndex.foreach { groupAndGroupNum =>
          val (group, groupNum) = groupAndGroupNum
          if (point.x >= group.box.minX - thresholdX &&
              point.x <= group.box.maxX + thresholdX &&
              point.centerY >= group.box.minY - thresholdY &&
              point.centerY <= group.box.maxY + thresholdY) {
            var minDistance = 999999
            group.points.foreach { point2 =>
              val deltaX = Math.abs(point.x - point2.x)
              val deltaY = Math.abs(point.centerY - point2.centerY) 
              val distance = deltaX + deltaY
              if (distance < minDistance &&
                  deltaX <= thresholdX && deltaY <= thresholdY) {
                minDistance = distance
              }
            }
            if (minDistance < minMinDistance) {
              minMinDistance = minDistance
              argminGroupNum = Some(groupNum)
            }
          }
        }
        if (minMinDistance > 0) {
          argminGroupNum.foreach { groupNum =>
            val group = expandedGroups(groupNum)
            val newX0 = point.x min group.box.minX
            val newX1 = point.x max group.box.maxX
            val newY0 = point.centerY min group.box.minY
            val newY1 = point.centerY max group.box.maxY
            val newBounds = BoundingBox(newX0, newX1, newY0, newY1)
            expandedGroups(groupNum) = Group(point :: group.points, newBounds)
          }
        }
      }
      expandedGroups.toList
    }

    def expandGroupsFromPoints(
        originalGroups:List[Group], allPoints:List[Point]) = {
      var groups = originalGroups
      allPoints.foreach { point =>
        var nearbyGroups = List[Group]()
        groups.foreach { group =>
          if (point.x >= group.box.minX - thresholdX &&
              point.x <= group.box.maxX + thresholdX &&
              point.centerY >= group.box.minY - thresholdY &&
              point.centerY <= group.box.maxY + thresholdY) {
            val pointMatch = group.points.exists { point2 =>
              Math.abs(point.x - point2.x) <= thresholdX &&
              Math.abs(point.centerY - point2.centerY) <= thresholdY
            }
            if (pointMatch) {
              nearbyGroups = group :: nearbyGroups
            }
          }
        }
  
        val newX0 = nearbyGroups.foldLeft(point.x) { _ min _.box.minX }
        val newX1 = nearbyGroups.foldLeft(point.x) { _ max _.box.maxX }
        val newY0 = nearbyGroups.foldLeft(point.centerY) { _ min _.box.minY }
        val newY1 = nearbyGroups.foldLeft(point.centerY) { _ max _.box.maxY }
        val newBounds = BoundingBox(newX0, newX1, newY0, newY1)
        val newPoints = nearbyGroups.foldLeft(List(point)) { _ ++ _.points }
        groups = Group(newPoints, newBounds) ::
          groups.filter { !nearbyGroups.contains(_) }
      }

      groups.flatMap { group =>
        val matchingOriginalGroups = originalGroups.filter { originalGroup =>
          originalGroup.points.intersect(group.points).size > 0
        }
        if (matchingOriginalGroups.size == 0) {
          List(group) // keep new group
        } else if (matchingOriginalGroups.size == 1) {
          List(group) // keep larger group
        } else { // matches multiple original groups
          expandExistingGroup(matchingOriginalGroups, group.points.toList)
        }
      }.filter { _.points.size > 5 }
    }

    def drawGroups(groups:List[Group], threshold:Int, stackHeight:Int) = {
      val demo5 = new ColorImage(image.w, image.h)
      val random = new Random()
      groups.foreach { group =>
        val r = random.nextInt(192) + 63
        val g = random.nextInt(192) + 63
        val b = random.nextInt(192) + 63
        group.points.foreach { point =>
          (point.x-0 to point.x+0).foreach { neighborX =>
            //(point.y-point.wavelen5/2 to point.y+point.wavelen5/2).foreach {
            (point.centerY-0 to point.centerY+0).foreach {
                neighborY =>
              if (neighborX >= 0 && neighborX < demo5.w &&
                  neighborY >= 0 && neighborY < demo5.h) {
                demo5(neighborX, neighborY) = (r, g, b)
              }
            }
          }
        }
      }
      demo5
    }

    var groups = List[Group]()
    (7 to 2 by -1).foreach { threshold =>
      (4 to 1 by -1).foreach { stackHeight =>
        val allPoints = gatherPoints(threshold, stackHeight)
        if (stackHeight == 4)
          groups = expandGroupsFromPoints(groups, allPoints)
        else
          groups = expandExistingGroup(groups, allPoints)
      }
    }
    val demo5 = drawGroups(groups, 2, 1)
    demo5.saveTo(new File(
      "demos/newstaff1.%s.%d.%d.png".format(caseName, 2, 1)))

    val demo6 = image.toColorImage
    groups.foreach { group =>
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        var maxThreshold = 0
        var maxStackHeight = 0
        var argmaxPoints = List[Point]()
        column.foreach { point =>
          //if (point.stackHeight > maxStackHeight ||
          //   (point.stackHeight == maxStackHeight &&
          //    point.threshold > maxThreshold)) {
          if (point.threshold > maxThreshold ||
             (point.threshold == maxThreshold &&
              point.stackHeight > maxStackHeight)) {
            maxThreshold = point.threshold
            maxStackHeight = point.stackHeight
            argmaxPoints = List(point)
          } else if (point.threshold == maxThreshold &&
                     point.stackHeight == maxStackHeight) {
            argmaxPoints = point :: argmaxPoints
          }
        }
        argmaxPoints.foreach { point =>
          point.ys.foreach { y =>
            val (r, g, b) = demo6(point.x, y)
//            demo6(point.x, y) = (255, g, b)
          }
        }
      }
    }
//    demo6.saveTo(new File(
//      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

/*
    class HLine(var points:List[(Int,Int)]) {}

    val joinX = 3
    val joinY = 0
//    val demo6 = image.toColorImage
    val window = 10
    groups.foreach { group =>
      var oldHLines = List[HLine]()
      var currentHLines = List[HLine]()
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(List[Int]()) { _ ++ _.ys }
        ys.foreach { y =>
          val hline = currentHLines.find { hline =>
            x - hline.points.head._1 <= joinX &&
            Math.abs(y - hline.points.head._2) <= joinY
          }
          hline match {
            case Some(hline) =>
              hline.points = (x, y) :: hline.points
            case None =>
              currentHLines = new HLine(List((x, y))) :: currentHLines
          }
        }
        val (replaceCurrentHLines, addToOldHLines) = currentHLines.partition {
          x - _.points.head._1 <= joinX
        }
        currentHLines = replaceCurrentHLines
        oldHLines ++= addToOldHLines
      }
      var allHlines = (currentHLines ++ oldHLines).sortBy { -_.points.size }

      val longest = allHlines.head
      var hlineToIndex = Map[HLine,Int](longest -> 0)
      var unassigned = allHlines.filter { _ != longest }
      var assignedButNotNeighbors = List(longest)
      var assignedAndNeighbors = List[HLine]()
      while (assignedButNotNeighbors.size > 0) {
        val hline1 = assignedButNotNeighbors.head
        assignedButNotNeighbors = assignedButNotNeighbors.tail
        val min1 = hline1.points.map { _._1 }.min
        val max1 = hline1.points.map { _._1 }.max
        val y1 = hline1.points.map { _._2 }.max
        val index1 = hlineToIndex(hline1)
        unassigned.foreach { hline2 =>
          val min2 = hline2.points.map { _._1 }.min
          val max2 = hline2.points.map { _._1 }.max
          val y2 = hline2.points.map { _._2 }.max
          if (min1 <= max2 && max1 >= min2) {
            if (y1 - y2 >= 4 && y1 - y2 <= 6) {
              hlineToIndex = hlineToIndex.updated(hline2, index1 + 1)
              unassigned = unassigned.filter { _ != hline2 }
              assignedButNotNeighbors = assignedButNotNeighbors ++ List(hline2)
            } else if (y2 - y1 >= 4 && y2 - y1 <= 6) {
              hlineToIndex = hlineToIndex.updated(hline2, index1 - 1)
              unassigned = unassigned.filter { _ != hline2 }
              assignedButNotNeighbors = assignedButNotNeighbors ++ List(hline2)
            }
          }
        }
      }

      allHlines.foreach { hline =>
        val rgb = hlineToIndex.get(hline) match {
          case Some(i) => if (i % 2 == 0) (255, 0, 0) else (0, 255, 0)
          case None => (0, 0, 255)
        }
        hline.points.foreach { xy =>
          val (x, y) = xy
          demo6(x, y) = rgb
        }
      }*/

/*    val window = 10
    groups.foreach { group =>
      val xToYs = new Array[Set[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      val realMinY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 99999 else ys.min
      }.min
      val realMaxY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 0 else ys.max
      }.max

      if (group.box.minX < 88 && group.box.maxX > 88 &&
          realMaxY > 78 && realMinY < 78) {
        (-120 to 0).foreach { adjustment =>
          val yToScore = new Array[Int](realMaxY + window + 1)
          (78 to 190).foreach { x =>
            xToYs(x).foreach { y =>
              val adjustmentY = (x - 78) * adjustment / 120
              if (y - adjustmentY < 101) {
                yToScore(y - adjustmentY) += 1
                //yToScore(y - adjustmentY + 1) += 1
              }
            }
          }
        if (yToScore.foldLeft(0) { _ + _ } > 0) {
            (0 until yToScore.size).foreach { y =>
              val v = yToScore(y) * 2
              demo6(78 + adjustment, y) = (v, v, 0)
            }
          }
        }
      }
    }*/

/*
    groups.foreach { group =>
      val xToYs = new Array[List[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(List[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      val realMaxY = xToYs.map { ys =>
        if (ys == null || ys.isEmpty) 0 else ys.max
      }.max

      val slopes = new Array[Int](group.box.maxX + 1)
      (group.box.minX to group.box.maxX - window).foreach { x0 =>
        val x1 = x0 + window
        var maxSumTop5 = 0
        var argmaxSlope = 0
        (2 to 2).foreach { slope =>
        //(-window to window).foreach { slope =>
          val yToScore = new Array[Int](realMaxY + window + 1)
          (0 until window).foreach { deltaX =>
            val x = x0 + deltaX
            xToYs(x).foreach { y =>
              val adjustedY = y - (slope * deltaX / window).intValue
              if (adjustedY >= 0) {
                yToScore(adjustedY) += 1
                yToScore(adjustedY + 1) += 1
              }
            }
          }
          val sumTop5 = yToScore.sorted.takeRight(5).sum
          if (sumTop5 > maxSumTop5) {
            maxSumTop5 = sumTop5
            argmaxSlope = slope
            (0 until realMaxY).foreach { y =>
              val v = (yToScore(y) * 5) min 255
              if (demo6(x0, y)._1 < v) {
                demo6(x0, y) = (v, v, 0)
              }
            }
          }
        }
        slopes(x0) = argmaxSlope
      }
    }
*/
/*
      (group.box.minX to group.box.maxX - window).foreach { x0 =>
        val neighborSlopes = (x0 until x0 + window).map { i => slopes(i) }
        val sumSlopes = neighborSlopes.foldLeft(0) { _ + _ }
        val meanSlope = sumSlopes / neighborSlopes.size
        val variance = neighborSlopes.map { slope =>
          (slope - meanSlope) * (slope - meanSlope)
        }.foldLeft(0) { _ + _ } / neighborSlopes.size.toFloat
        if (variance <= 2.0f) {
          (-10 to slopes(x0)).foreach { y =>
            demo6(x0 + window/2, realMaxY - 10 + y) = (255, 255, 0)
          }
        }
      }
*/
//    demo6.saveTo(new File(
//      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

//println(groups.map { _.points.filter { _.x == 83 }.sortBy { point =>
//  (point.threshold, point.stackHeight)
//}})

    var groupNumToXToYs = new Array[Array[Set[Int]]](groups.size)
    (0 until groups.size).foreach { groupNum =>
       val group = groups(groupNum)
       val xToYs = new Array[Set[Int]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        xToYs(x) = ys
      }
      groupNumToXToYs = groupNumToXToYs.updated(groupNum, xToYs)
    }

    // 999999 means uninitialized
    val groupNumToXToMinY = (0 until groups.size).toArray.map { groupNum =>
      (0 until image.w).map { x => 999999 }.toArray
    }
    // -1 means uninitialized
    val groupNumToXToMaxY = (0 until groups.size).toArray.map { groupNum =>
      (0 until image.w).map { x => -1 }.toArray
    }

    (0 until image.w).foreach { x =>
      var groupNumToYs = new Array[Set[Int]](groups.size)
      groups.zipWithIndex.foreach { groupAndGroupNum =>
        val (group, groupNum) = groupAndGroupNum
        val column = group.points.filter { point => point.x == x }
        val ys = column.foldLeft(Set[Int]()) { _ ++ _.ys }
        groupNumToYs(groupNum) = ys
      }

      (0 until image.h).foreach { y =>
        var minDistance = 999999
        var argmaxGroupNum:Option[Int] = None
        (0 until groups.size).foreach { groupNum =>
          groupNumToYs(groupNum).foreach { y2 =>
            val distance = Math.abs(y2 - y)
            if (distance < minDistance) {
              minDistance = distance
              argmaxGroupNum = Some(groupNum)
            }
          }
        }
        argmaxGroupNum.foreach { groupNum =>
          groupNumToXToMinY(groupNum)(x) = groupNumToXToMinY(groupNum)(x) min y
          groupNumToXToMaxY(groupNum)(x) = groupNumToXToMaxY(groupNum)(x) max y
        }
      }
    }

    val staffStrength = new GrayImage(image.w, image.h)
    val bestWavelen5 = new GrayImage(image.w, image.h)
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      (15 to 28).foreach { wavelen5 =>
        (group.box.minX until group.box.maxX).foreach { x =>
          val minY = groupNumToXToMinY(groupNum)(x)
          val maxY = groupNumToXToMaxY(groupNum)(x)
  
          (minY to maxY).foreach { centerY =>
            val insidePoints = List(-4, -2, 0, 2, 4).map { i =>
              demo(x, centerY + Math.floor(wavelen5 * i/10.0f).intValue) max
              demo(x, centerY + Math.floor(wavelen5 * i/10.0f).intValue + 1)
            }.toArray
            val meanInside = insidePoints.min
  
            val oldV = staffStrength(x, centerY)
            if (meanInside > oldV) {
              staffStrength(x, centerY) = meanInside
              bestWavelen5(x, centerY) = wavelen5
            }
          }
        }
      }
    }

    val howClose = 10 // number of neighbors to the left and right to consider
    def getNeighborPoints(xToBestY:Array[Int], x:Int) = {
      val leftPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeLeftPoints = leftPoints.slice(0, x
        ).filter { _._2 != 0 }.takeRight(howClose).toList
      val rightPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeRightPoints = rightPoints.slice(x + 1, xToBestY.size
        ).filter { _._2 != 0 }.take(howClose).toList
      (closeLeftPoints, closeRightPoints)
    }

    val demo7 = staffStrength.scaleValueToMax255.toColorImage
    val groupNumToXToBestY = new Array[Array[Int]](groups.size)
    val groupNumToXToBestWavelen5 = new Array[Array[Int]](groups.size)
    val random = new Random()
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      val rgb = (random.nextInt(16) + 15,
                 random.nextInt(16) + 15,
                 random.nextInt(16) + 15)
      (group.box.minX to group.box.maxX).foreach { x =>
        val minY = groupNumToXToMinY(groupNum)(x)
        val maxY = groupNumToXToMaxY(groupNum)(x)
        (minY to maxY).foreach { y =>
          val rgbOld = demo7(x, y)
          demo7(x, y) = ((rgbOld._1 + rgb._1) min 255,
                         (rgbOld._2 + rgb._2) min 255,
                         (rgbOld._3 + rgb._3) min 255)
        }
      }

      val xToVYPairs = new Array[Array[(Int,Int)]](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        val minY = groupNumToXToMinY(groupNum)(x)
        val maxY = groupNumToXToMaxY(groupNum)(x)

        if (maxY - minY > 4) {
          var vyPairs = new Array[(Int,Int)](maxY - minY + 1)
          (minY to maxY).foreach { y =>
            vyPairs(y - minY) = (staffStrength(x, y), y)
          }
          Sorting.quickSort(vyPairs)
          xToVYPairs(x) = vyPairs
        }
      }

      val xToBestY = new Array[Int](group.box.maxX + 1)
      val xToBestWavelen5 = new Array[Int](group.box.maxX + 1)
      var threshold = staffStrength.data.max
      while (threshold > 0) {
        (group.box.minX to group.box.maxX).foreach { x =>
          val vyPairs = xToVYPairs(x)
          if (vyPairs != null && xToBestY(x) == 0) {
            val (max1V, max1Y) = vyPairs(vyPairs.size - 1)
            val (max2V, max2Y) = vyPairs(vyPairs.size - 2)
            val (max3V, max3Y) = vyPairs(vyPairs.size - 3)
            val (max4V, max4Y) = vyPairs(vyPairs.size - 4)
            val (maxV, bestY, nextBestV) =
              // if the 3 brightest are adjacent, average them and take middle
              if ((max1Y max max2Y max max3Y) -
                  (max1Y min max2Y min max3Y) == 2)
                ((max1V + max2V + max3V) / 3,
                 (max1Y min max2Y min max3Y) + 1,
                 max4V)
              // if the 2 brightest are adjacent, average them and take lower
              else if ((max1Y max max2Y) - (max1Y min max2Y) == 1)
                ((max1V + max2V) / 2, max1Y min max2Y, max3V)
              // otherwise pick the brightest
              else
                (max1V, max1Y, max2V)
            if (maxV - nextBestV >= threshold) {
              // now look at its neighbors to see if they form a line
              val (leftPoints, rightPoints) = getNeighborPoints(xToBestY, x)
              val errorYOverX =
                if (leftPoints.size + rightPoints.size > 1) {
                  val (slope, intercept) =
                    linearRegression(leftPoints ++ rightPoints)
                  val predictedY = slope * x + intercept
                  val errorY = Math.abs(predictedY - bestY)
                  val minXDistance = 
                    if (leftPoints.size > 0 && rightPoints.size > 0) {
                      Math.abs(leftPoints(leftPoints.size - 1)._1 - x) min
                        Math.abs(rightPoints(0)._1 - x)
                    } else if (leftPoints.size > 0) {
                      Math.abs(leftPoints(leftPoints.size - 1)._1 - x)
                    } else if (rightPoints.size > 0) {
                      Math.abs(rightPoints(0)._1 - x)
                    } else 999999
//println((errorY / minXDistance, errorY, minXDistance,
//  leftPoints, rightPoints, x, bestY))
                  errorY / minXDistance
                } else {
                  0.0
                }
              if (errorYOverX < 0.5) {
                xToBestY(x) = bestY
                xToBestWavelen5(x) = bestWavelen5(x, bestY)
                demo7(x, bestY) = (0, 255, 0)
              }
            } // end if meets threshold
          } // end if has point in x
        } // next x
        threshold -= 1
      } // next threshold
      groupNumToXToBestY(groupNum) = xToBestY
      groupNumToXToBestWavelen5(groupNum) = xToBestWavelen5
    } // next group


    // now fill in the gaps
/*
    val groupNumToFullXToBestY = new Array[Array[Int]](groups.size)
    val requiredDensity = 0.3f
    val groupNumToRealMinX = new Array[Int](groups.size)
    (0 until groups.size).foreach { i => groupNumToRealMinX(i) = 999999 }
    val groupNumToRealMaxX = new Array[Int](groups.size)
    groups.zipWithIndex.foreach { groupAndGroupNum =>
      val (group, groupNum) = groupAndGroupNum
      val xToBestY = groupNumToXToBestY(groupNum)
      val fullXToBestY = new Array[Int](group.box.maxX + 1)
      (group.box.minX to group.box.maxX).foreach { x =>
        if (xToBestY(x) == 0) {
          val (leftPoints, rightPoints) = getNeighborPoints(xToBestY, x)
          if (leftPoints.size > 0 && rightPoints.size > 0) {
            val leftmostX = leftPoints(0)._1
            val rightmostX = rightPoints(rightPoints.size - 1)._1
            if (leftPoints.size >= (x - leftmostX) * requiredDensity &&
                rightPoints.size >= (rightmostX - x) * requiredDensity) {
              val (slope, intercept) =
                linearRegression(leftPoints ++ rightPoints)
              val predictedY = Math.round(slope * x + intercept).intValue
              fullXToBestY(x) = predictedY
              groupNumToRealMinX(groupNum) = groupNumToRealMinX(groupNum) min x
              groupNumToRealMaxX(groupNum) = groupNumToRealMaxX(groupNum) max x
              demo7(x, predictedY) = (0, 0, 255)
            }
          }
        } else {
          fullXToBestY(x) = xToBestY(x)
        }
      }
      groupNumToFullXToBestY(groupNum) = fullXToBestY
    }*/
    demo7.scaleValueToMax255.saveTo(new File(
      "demos/newstaff.%s.%d.%d.png".format(caseName, 2, 1)))

    val denseGroupNums = (0 until groups.size).filter { groupNum =>
      val requiredDensity = 0.1f
      val numPoints = groupNumToXToBestY(groupNum).filter { _ != 0 }.size
      //println((numPoints, image.w, numPoints / image.w.toFloat))
      numPoints / image.w.toFloat >= requiredDensity
    }.toArray

    //(groupNumToXToBestY, groupNumToXToBestWavelen5)
    (denseGroupNums.map { groupNum => groupNumToXToBestY(groupNum) },
     denseGroupNums.map { groupNum => groupNumToXToBestWavelen5(groupNum) })
  }

  // returns (slope, intercept)
  def linearRegression(points:List[(Int,Int)]) : (Float, Float) = {
    val n = points.size
    var sumX = 0
    var sumY = 0
    var sumXY = 0
    var sumXX = 0
    points.foreach { xy =>
      val (x, y) = xy
      sumX += x
      sumY += y
      sumXY += x * y
      sumXX += x * x
    }
    val slope = (n * sumXY - sumX * sumY) /
                (n * sumXX - sumX * sumX).floatValue
    val intercept = (sumY - slope * sumX) / n.floatValue
    (slope, intercept)
  }

  def quarterSize(image:GrayImage) = {
    val output = new GrayImage(image.w / 2, image.h / 2)
    (0 until output.w).foreach { x =>
      (0 until output.h).foreach { y =>
        output(x, y) = (
          image(x*2 - 2, y*2 - 2) *  1 +
          image(x*2 - 1, y*2 - 2) *  4 +
          image(x*2 + 0, y*2 - 2) *  6 +
          image(x*2 + 1, y*2 - 2) *  4 +
          image(x*2 + 2, y*2 - 2) *  1 +
          image(x*2 - 2, y*2 - 1) *  4 +
          image(x*2 - 1, y*2 - 1) * 16 +
          image(x*2 + 0, y*2 - 1) * 24 +
          image(x*2 + 1, y*2 - 1) * 16 +
          image(x*2 + 2, y*2 - 1) *  4 +
          image(x*2 - 2, y*2 + 0) *  6 +
          image(x*2 - 1, y*2 + 0) * 24 +
          image(x*2 + 0, y*2 + 0) * 36 +
          image(x*2 + 1, y*2 + 0) * 24 +
          image(x*2 + 2, y*2 + 0) *  6 +
          image(x*2 - 2, y*2 + 1) *  4 +
          image(x*2 - 1, y*2 + 1) * 16 +
          image(x*2 + 0, y*2 + 1) * 24 +
          image(x*2 + 1, y*2 + 1) * 16 +
          image(x*2 + 2, y*2 + 1) *  4 +
          image(x*2 - 2, y*2 + 2) *  1 +
          image(x*2 - 1, y*2 + 2) *  4 +
          image(x*2 + 0, y*2 + 2) *  6 +
          image(x*2 + 1, y*2 + 2) *  4 +
          image(x*2 + 2, y*2 + 2) *  1 +
          0) / 256
      }
    }
    output
  }

  def classifyGoodOrBad(staffs16:Array[Array[Int]], image:GrayImage,
      demo:ColorImage) {
    val howClose = 2 // number of neighbors to the left and right to consider
    def getNeighborPoints(xToBestY:Array[Int], x:Int) = {
      val leftPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeLeftPoints = leftPoints.slice(0, x
        ).filter { _._2 != 0 }.takeRight(howClose).toList
      val rightPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeRightPoints = rightPoints.slice(x + 1, xToBestY.size
        ).filter { _._2 != 0 }.take(howClose).toList
      (closeLeftPoints, closeRightPoints)
    }

    val wavelen5 = 57
    val random = new Random()
    staffs16.foreach { xToY =>
      (0 until xToY.size * 4).foreach { x =>
        val xDiv4 = x / 4
        val yDiv4 = xToY(xDiv4)
        val centerY = yDiv4 * 4 + 2
/*        if (yDiv4 > 0) {
          var blackVs = List[Int]()
          var whiteVs = List[Int]()
          List(-4, -2, 0, 2, 4).foreach { staffY =>
            val y = centerY + Math.round(wavelen5 * staffY / 10).intValue
            blackVs = image(x, y) :: blackVs
          }
          List(-5, -3, -1, 1, 3, 5).foreach { staffY =>
            val y = centerY + Math.round(wavelen5 * staffY / 10).intValue
            whiteVs = image(x, y) :: whiteVs
          }
          val meanWhite = whiteVs.sum / whiteVs.size.toFloat
          val varianceWhite = whiteVs.map { v =>
            Math.pow(meanWhite - v, 2) }.sum / whiteVs.size.toFloat
          val stdDevWhite = Math.sqrt(varianceWhite)
          //(-wavelen5/2 to wavelen5/2).foreach { y =>
          //val v = (meanWhite + random.nextGaussian() * stdDevWhite).intValue
          //demo(x, centerY + y) = (v, v, v)
          //}
          if (stdDevWhite >= 10) {
            (-wavelen5/2 to wavelen5/2).foreach { y =>
              demo(x, centerY + y) = (255, 0, 0)
            }
          }
        } // end if */
//        if (yDiv4 == 0) { // do we need to interpolate?
          val (leftPoints, rightPoints) = getNeighborPoints(xToY, xDiv4)
          if (leftPoints.size > 0 && rightPoints.size > 0) {
            var meanWhiteVs = List[Float]()
            var stdDevWhiteVs = List[Float]()
            var meanBlackVs = List[Float]()
            var stdDevBlackVs = List[Float]()
            (leftPoints ++ rightPoints).foreach { xyDiv4 =>
              var blackVs = List[Int]()
              var whiteVs = List[Int]()
              val (x2, centerY2) = (xyDiv4._1 * 4 + 2, xyDiv4._2 * 4 + 2)
              List(-4, -2, 0, 2, 4).foreach { staffY =>
                val y2 = centerY2 + Math.round(wavelen5 * staffY / 10).intValue
                var minV = 999999
                ((y2 - 2 max 0) to (y2 + 2 min image.h - 1)).foreach { y3 =>
                  val v = image(x2, y3)
                  if (v < minV)
                    minV = v
                }
                blackVs = minV :: blackVs
              }
              List(-5, -3, -1, 1, 3, 5).foreach { staffY =>
                val y2 = centerY2 + Math.round(wavelen5 * staffY / 10).intValue
                whiteVs = image(x2, y2) :: whiteVs
              }
              val meanWhite = whiteVs.sum / whiteVs.size.toFloat
              val varianceWhite = whiteVs.map { v =>
                Math.pow(meanWhite - v, 2) }.sum / whiteVs.size.toFloat
              val stdDevWhite = Math.sqrt(varianceWhite).toFloat
              val meanBlack = blackVs.sum / blackVs.size.toFloat
              val varianceBlack = blackVs.map { v =>
                Math.pow(meanBlack - v, 2) }.sum / blackVs.size.toFloat
              val stdDevBlack = Math.sqrt(varianceBlack).toFloat
              if (stdDevWhite < 10) {
                meanWhiteVs = meanWhite :: meanWhiteVs
                stdDevWhiteVs = stdDevWhite :: stdDevWhiteVs
                meanBlackVs = meanBlack :: meanBlackVs
                stdDevBlackVs = stdDevBlack :: stdDevBlackVs
              }
            }
            if (meanWhiteVs.size > 0) {
              val meanWhite = meanWhiteVs.sum / meanWhiteVs.size.toFloat
              val stdDevWhite = stdDevWhiteVs.sum / stdDevWhiteVs.size.toFloat
              val meanBlack = meanBlackVs.sum / meanBlackVs.size.toFloat
              val stdDevBlack = stdDevBlackVs.sum / stdDevBlackVs.size.toFloat

              val (slope, intercept) =
                linearRegression(leftPoints ++ rightPoints)
              val predictedYDiv4 =
                Math.round(slope * xDiv4 + intercept).intValue
              (-wavelen5/2 to wavelen5/2).foreach { yNeighbor =>
                val staffYOver2 = yNeighbor * 5.0f / wavelen5
                val closenessToLine =
                  Math.abs(staffYOver2 - Math.round(staffYOver2))
                val darkestExpectedV =
                  if (closenessToLine > 0.25) meanWhite - stdDevWhite * 3
                  else meanBlack - stdDevBlack * 2
                val y = predictedYDiv4 * 4 + 2 + yNeighbor
                if (image(x, y) < darkestExpectedV) {
                  demo(x, y) = (255, 0, 0)
                }
              }
            } // end if
          } // end if
//        } // end if
      } // next x
    } // next staff
  }

  def improveStaffs(staffs:Array[Array[Int]], wavelen5s:Array[Array[Int]],
      image:GrayImage, demo:ColorImage) = {
    val howClose = 10 // number of neighbors to the left and right to consider
    def getNeighborPoints(xToBestY:Array[Int], x:Int) = {
      val leftPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeLeftPoints = leftPoints.slice(0, x
        ).filter { _._2 != 0 }.takeRight(howClose).toList
      val rightPoints =
        xToBestY.zipWithIndex.map { yx => (yx._2, yx._1) }
      val closeRightPoints = rightPoints.slice(x + 1, xToBestY.size
        ).filter { _._2 != 0 }.take(howClose).toList
      (closeLeftPoints, closeRightPoints)
    }

    staffs.zipWithIndex.map { staffAndStaffNum =>
      val (xToY, staffNum) = staffAndStaffNum
      val xToWavelen5 = wavelen5s(staffNum)

      var minSumDifference = 999999.0f
      var argmaxLeftWavelen5 = 0
      var argmaxRightWavelen5 = 0
      (15 to 130).foreach { leftWavelen5 =>
        (15 to 130).foreach { rightWavelen5 =>
          var sumDifference = 0.0f
          (0 until xToWavelen5.size).foreach { x =>
            val actualWavelen5 = xToWavelen5(x)
            if (actualWavelen5 != 0) {
              val progress = x / xToWavelen5.size.toFloat
              val predictedWavelen5 =
                leftWavelen5 + (rightWavelen5 - leftWavelen5) * progress
              sumDifference += Math.abs(predictedWavelen5 - actualWavelen5)
            }
          }
          if (sumDifference < minSumDifference) {
            minSumDifference = sumDifference
            argmaxLeftWavelen5 = leftWavelen5
            argmaxRightWavelen5 = rightWavelen5
          }
        }
      }

      (0 until xToY.size).toArray.map { x =>
        //demo(x, xToWavelen5(x)) = (0, 0, 255)
        //demo(x + 1, xToWavelen5(x)) = (0, 0, 255)
        val (leftPoints, rightPoints) = getNeighborPoints(xToY, x)
        if (leftPoints.size > 0 || rightPoints.size > 0) {
          val (slope, intercept) =
            linearRegression(leftPoints ++ rightPoints)
          val predictedCenterY = Math.round(slope * x + intercept).intValue
          val progress = x / xToY.size.toFloat
          val predictedWavelen5 = Math.round(argmaxLeftWavelen5 +
            (argmaxRightWavelen5 - argmaxLeftWavelen5) * progress).intValue
          //demo(x, predictedWavelen5) = (0, 0, 255)

          var minMaxV = 999999
          var argmaxCenterY = 0.0f
          var argmaxWavelen5 = 0.0f
          (predictedWavelen5 - 5 to predictedWavelen5 + 5).foreach { wavelen5 =>
            var centerY = predictedCenterY + -5.0f
            while (centerY < predictedCenterY + 5.0f) {
              val insidePoints = List(-4, -2, 0, 2, 4).map { i =>
                image(x, Math.floor(centerY + wavelen5 * i/10.0f).intValue) min
                image(x, Math.floor(centerY + wavelen5 * i/10.0f).intValue + 1)
              }.toArray
              val maxV = insidePoints.max
              if (maxV < minMaxV) {
                minMaxV = maxV
                argmaxCenterY = centerY
                argmaxWavelen5 = wavelen5
              }
  
              centerY += 0.2f
            }
          }

          List(-4, -2, 0, 2, 4).foreach { i =>
            demo(x, Math.round(argmaxCenterY + argmaxWavelen5 * i/10.0f
              ).intValue) = (255, 255, 0)
          }

           0 //Math.round(argmaxCenterY).intValue
        } else {
          0
        }
      }
    }
  }

  def processCase(caseName:String) : Performance = {
    var casePerformance = Performance(Set(), Set(), Set())

    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage
    val annotationPath = "input/%s.json".format(caseName)
    val annotationString = scala.io.Source.fromFile(annotationPath).mkString
    //val annotationBoxes = loadAnnotationsJson(annotationString)
/*    val imageShrunken = caseName match {
      case "2" => quarterSize(quarterSize(image))
      case "4" => quarterSize(image)
      case "1320610762.M373157P72050Q0R91a6ef26faeb752b.macmini.0" => image
      case "1320611573.M358927P73155Q0Raf96d58b25a32902.macmini.0" => image
      case "1320613796.M254332P75976Q0R61fcb171586b3dba.macmini.0" =>
        quarterSize(quarterSize(image))
      case "1320618283.M681674P81510Q0R96e43bc997510706.macmini.0" =>
        quarterSize(quarterSize(image))
      case "1321227085.M56099P82785Q0R6b4534ebfa16e985.macmini.0" =>
        quarterSize(image)
      case "photo1" => quarterSize(image)
      case "photo2" => quarterSize(image)
      case "photo3" => quarterSize(image)
      case _ => image
    }*/

    def drawStaffs(xToYArrays:Array[Array[Int]], multiplier:Int,
        rgb:(Int,Int,Int), out:ColorImage) {
      xToYArrays.foreach { xToY =>
        (0 until xToY.size).foreach { x =>
          val y = xToY(x)
          if (y != 0) {
            out(x * multiplier, y * multiplier) = rgb
          }
        }
      }
    }

    def multiplyStaffs(staffs:Array[Array[Int]], multiplier:Int) = {
      staffs.map { oldXToY =>
        val newXToY = new Array[Int](oldXToY.size * (multiplier + 1))
        oldXToY.zipWithIndex.foreach { yx =>
          val (oldY, oldX) = yx
          if (oldY != 0) {
            newXToY(oldX * multiplier + (multiplier / 2)) =
              oldY * multiplier + (multiplier / 2)
          }
        }
        newXToY
      }
    }

    def detectStaffsAndMultiply(
        imageShruken:GrayImage, caseName:String, multiplier:Int) = {
      val (staffs, wavelen5s) = detectStaffs(imageShruken, caseName)
      val newStaffs = multiplyStaffs(staffs, multiplier)
      val newWavelen5s = multiplyStaffs(wavelen5s, multiplier)
      (newStaffs, newWavelen5s)
    }

    val demo = image.toColorImage
    val image16 = quarterSize(quarterSize(image))
//    val image4 = quarterSize(image)
    val (staffs16, wavelen5s16) = detectStaffsAndMultiply(image16, caseName, 4)
//  val staffs4 = detectStaffs(image4, caseName)
//    val staffs1 =
//      if (image.w + image.h < 1500)
//        detectStaffs(image, caseName)
//      else
//        Array[Array[Int]]()
//    drawStaffs(staffs16, 4, (255, 0, 0), demo)
//    drawStaffs(staffs4, 2, (0, 255, 0), demo)
//    drawStaffs(staffs1, 1, (0, 0, 255), demo)
//    classifyGoodOrBad(staffs16, image, demo)
    drawStaffs(staffs16, 1, (255, 0, 0), demo)
    val staffsNew = improveStaffs(staffs16, wavelen5s16, image, demo)
    drawStaffs(staffsNew, 1, (0, 255, 0), demo)
    demo.saveTo(new File("demos/all_staffs.%s.png".format(caseName)))
    
/*
    val midlinesPath = new File("output/midlines/%s.png".format(caseName))
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

    val erasedPath = new File("output/erased/%s.jpeg".format(caseName))
    val justNotes = readOrGenerate(erasedPath, saveGrayImage, loadGrayImage) {
      () => EraseStaff.run(image, staffs, caseName)
    }

    val beamsPath = new File("output/beams/%s.json".format(caseName))
    val beams = readOrGenerate(beamsPath, saveBeams, loadBeams) { () =>
      FindBeams.run(justNotes, image, staffs, caseName)
    }

    val noBeamsPath = new File("output/no_beams/%s.jpeg".format(caseName))
    val justNotesNoBeams =
        readOrGenerate(noBeamsPath, saveGrayImage, loadGrayImage) { () =>
      EraseBeams.run(justNotes, beams, staffs, caseName)
    }

    val vSlopeRangePath =
      new File("output/v_slope_range/%s.json".format(caseName))
    val vSlopeRange = readOrGenerate(vSlopeRangePath,
        saveFloatPair, loadFloatPair) { () =>
      FindVSlopeRange.run(justNotes, image, staffs, caseName)
    }

    var allStaffsPredictedNotes = List[Set[TemplateMatch]]()
    staffs.zipWithIndex.foreach { staffAbsoluteAndI =>
      val (staffAbsolute, i) = staffAbsoluteAndI
      val staffName = staffAbsolute.staffName
      val (x0, x1) = (staffAbsolute.bounds.minX, staffAbsolute.bounds.maxX)
      val (y0, y1) = (staffAbsolute.bounds.minY, staffAbsolute.bounds.maxY)
      val (w, h) = (x1 - x0 + 1, y1 - y0 + 1)
      val annotationBox =
        (if (i < annotationBoxes.size) Some(annotationBoxes(i)) else None)

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
      val justNotesCropped = justNotes.crop(x0, y0, w, h)
      val justNotesNoBeamsCropped = justNotesNoBeams.crop(x0, y0, w, h)
      val (m0, m1) = vSlopeRange
      val vSlopeRangeCropped = (m0 + (m1 - m0) * (x0 / image.w.floatValue),
                                m0 + (m1 - m0) * (x1 / image.w.floatValue))

      val vLinesPath = new File("output/v_lines/%s.json".format(staffName))
      val vLines = readOrGenerate(vLinesPath, saveVLines, loadVLines) { () =>
        FindVLines.run(justNotesCropped, cropped, vSlopeRangeCropped, staffName)
      }

      val transformPath = new File(
        "output/orthonormal_transform/%s.json".format(staffName))
      val transform = readOrGenerate(transformPath, saveOrthonormalTransform,
          loadOrthonormalTransform) { () =>
        SetupOrthonormalTransform.run(
          justNotesNoBeamsCropped.w, justNotesNoBeamsCropped.h,
          staffRelative, vSlopeRangeCropped, staffName)
      }

      val orthonormalImagePath = new File(
        "output/orthonormal_image/%s.png".format(staffName))
      val orthonormalImage = readOrGenerate(
          orthonormalImagePath, saveGrayImage, loadGrayImage) { () =>
        DoOrthonormalTransform.run(justNotesNoBeamsCropped, transform)
      }

      val slicesPath = new File("output/slices/%s.json".format(staffName))
      val boxToChildBoxes = readOrGenerate(
          slicesPath, saveBoxesMap, loadBoxesMap) { () =>
        FindVerticalSlices.run(orthonormalImage, transform, vLines, staffName)
      }
      val verticalSlicesDemo =
        demoVerticalSlices(orthonormalImage, boxToChildBoxes, staffName)

      val filledBoxesPath =
        new File("output/filled_boxes/%s.json".format(staffName))
      val filledBoxes = readOrGenerate(
          filledBoxesPath, saveFilledBoxes, loadFilledBoxes) { () =>
        FillBoxes.run(boxToChildBoxes, orthonormalImage, transform, staffName)
      }

      val consideredNotes = allTemplatesFromBoxes(filledBoxes)
      val predictedNotes = chooseBestNotes(filledBoxes, orthonormalImage)
  
      val performance = calcPerformance(predictedNotes,
        annotationBox.foldLeft(List[Set[LabeledPoint]]()) { _ ++ _.pointGroups})
      outputPerformance(performance, staffName)
      demoPredictedNotes(predictedNotes, consideredNotes, orthonormalImage,
        performance, staffAbsolute.bounds, transform,
        verticalSlicesDemo.copy, staffName)
      allStaffsPredictedNotes ++= predictedNotes
  
      casePerformance += performance
    } // next staff

    writeToMidi(allStaffsPredictedNotes, caseName)

*/
    casePerformance
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

      println(Colors.ansiEscapeNormal)
      System.exit(0)
    } catch {
      case e:Throwable =>
        e.printStackTrace()
        println(Colors.ansiEscapeNormal)
        System.exit(1)
    }
  }
}
