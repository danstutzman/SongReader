import com.twitter.json.Json
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import java.lang.Math
import javax.imageio.ImageIO
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
  val x:Int,
  val y:Int,
  val staffY:Int
) {}

case class ExpectedNote (
  val label:String,
  val staffY:Int
) {}

case class AnnotationBox (
  val num:Int,
  val left:Int,
  val top:Int,
  val width:Int,
  val height:Int,
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
      val points = 
        box("points").asInstanceOf[List[Map[String,Any]]].map { point =>
          LabeledPoint(
            point("type").asInstanceOf[String],
            point("x").asInstanceOf[Int],
            point("y").asInstanceOf[Int],
            point("staffY").asInstanceOf[Int]
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

  def processCase(caseName:String) : Performance = {
    var casePerformance = Performance(Set(), Set(), Set())

    val imagePath = new File("input/%s.jpeg".format(caseName))
    val image = ColorImage.readFromFile(imagePath).toGrayImage
    val annotationPath = "input/%s.json".format(caseName)
    val annotationString = scala.io.Source.fromFile(annotationPath).mkString
    val annotationBoxes = loadAnnotationsJson(annotationString)
    val allAnnotationNoteGroups:List[Set[ExpectedNote]] =
      annotationBoxes.foldLeft(List[Set[ExpectedNote]]()) { _ ++ _.notes }

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

    var i = 0
    staffs.foreach { staffAbsolute =>
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
      demoVerticalSlices(orthonormalImage, boxToChildBoxes, staffName)

      val filledBoxesPath =
        new File("output/filled_boxes/%s.json".format(staffName))
      val filledBoxes = readOrGenerate(
          filledBoxesPath, saveFilledBoxes, loadFilledBoxes) { () =>
        FillBoxes.run(boxToChildBoxes, orthonormalImage, transform, staffName)
      }

      val predictedNotes = filledBoxes.map { box =>
        if (box.templates.size > 0)
          box.templates
        else
          box.childBoxes.foldLeft(Set[TemplateMatch]()) { (accum, childBox) =>
            accum ++ chooseBestOverlappingSets(childBox.box, childBox.templates,
              new TemplateResizer(), orthonormalImage)
          }
      }
  
      println("  calcPerformance")
      val performance = calcPerformance(predictedNotes,
        annotationBox.foldLeft(List[Set[ExpectedNote]]()) { _ ++ _.notes })
      println("Case %2s: precision: %.3f, recall: %.3f".format(
        caseName, performance.precision, performance.recall))
      printf("   correct: %s\n", performance.correctNotes.toString.replaceAll(
          "\\)\\), \\(", ")),\n                ("))
      if (performance.spuriousNotes.size > 0)
        printf("  spurious: %s\n", performance.spuriousNotes.toString.
          replaceAll("\\)\\), \\(", ")),\n                ("))
      if (performance.missingNotes.size > 0)
        printf("   missing: %s\n", performance.missingNotes.toString.replaceAll(
          "\\)\\), \\(", ")),\n                ("))
  
      val demo = orthonormalImage.toColorImage
      val templateResizer = new TemplateResizer()
      predictedNotes.foldLeft(Set[TemplateMatch]()) { _ ++ _ }.foreach { point=>
        val color =
          if (performance.correctNotes.filter{ _._2 == point }.size > 0)
            (0, 128, 0)
          else
            (255, 0, 0) // spurious
          drawTemplateMatch(point, demo, templateResizer(point), color)
      }
      demo.saveTo(new File("demos/notes.%s.png".format(staffName)))
  
      casePerformance = Performance(
        casePerformance.correctNotes ++ performance.correctNotes,
        casePerformance.spuriousNotes ++ performance.spuriousNotes,
        casePerformance.missingNotes ++ performance.missingNotes)
      i += 1
    } // next staff

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

    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      println(Colors.ansiEscapeNormal)
    }
    System.exit(0)
  }
}
