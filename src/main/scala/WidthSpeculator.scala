import java.io.File
import scala.collection.immutable.TreeMap

sealed abstract class Warning {}

case class IncompleteMeasure(
  val context:Context,
  val expectedNumBeats:Float,
  val actualNumBeats:Float
) extends Warning {}

case class MultipleClefs (val context:Context) extends Warning {}

case class Context (
  val hadMeasure:Boolean, // referring to the vertical line before the clef
  val position:Int,
  val numBeatsPerMeasure:Float,
//  val nextStartingSymbolExpected
//  val hadClef:Boolean,
//  val had44TimeSig:Boolean,
  val numBeatsInMeasure:Float,
  //val hadClosingLine:Boolean
  val warnings:List[Warning]
) {
  def addClef = {
    Context(hadMeasure, position + 1, numBeatsPerMeasure, numBeatsInMeasure,
      warnings)
  }
  def addTimeSig(newNumBeatsPerMeasure:Float) = {
    Context(hadMeasure, position + 1, newNumBeatsPerMeasure, numBeatsInMeasure,
      warnings)
  }
  def addBeats(numNewBeats:Float) = {
    val newWarnings = warnings
    Context(hadMeasure, position + 1, numBeatsPerMeasure,
      numBeatsInMeasure + numNewBeats, newWarnings)
  }
  def addMeasure = {
    if (hadMeasure) {
      val newWarnings =
        if (numBeatsInMeasure < numBeatsPerMeasure)
          IncompleteMeasure(this, numBeatsPerMeasure, numBeatsInMeasure) ::
            warnings
        else
          warnings
      Context(hadMeasure, position + 1, numBeatsPerMeasure, 0f, newWarnings)
    } else {
      Context(true, position + 1, numBeatsPerMeasure, 0f, warnings)
    }
  }
}

object Context {
  val startingContext = Context(false, 0, 0f, 0f, List[Warning]())
}


object WidthSpeculator {
  val easyCaseNames = Set("3a")

  def readAnnotations() = {
    var caseNameToSymbols = TreeMap[String,Array[String]]()
    val annotationPath = "input/width_labels.txt"
    val annotationString = scala.io.Source.fromFile(annotationPath).mkString
    val lines = annotationString.split("\n")
    (0 until lines.size by 2).foreach { i =>
      val caseName = lines(i)
      val symbols = lines(i + 1).split(" ")
      caseNameToSymbols = caseNameToSymbols.updated(caseName, symbols)
    }
    caseNameToSymbols
  }

  def findWarnings(symbols:List[String], c:Context) : Context = {
    symbols match {
      case Nil => c
      case symbol :: otherSymbols =>
        val newContext = symbol match {
          case "|"  => c.addMeasure
          case "G"  => c.addClef
          case "44" => c.addTimeSig(4.0f)
          case "2|" => c.addBeats(2f)
          case "4|" => c.addBeats(1f)
          case "8|" => c.addBeats(0.5f)
        }
        findWarnings(otherSymbols, newContext)
    }
  }

  def readWidthPairs(caseName:String) = {
    val path = "output/widths/%s.txt".format(caseName)
    val contents = scala.io.Source.fromFile(path).mkString
    val allLines = contents.split("\n")
    val (header, lines) = (allLines.head, allLines.tail)
    var lastX = 0
    lines.reverse.map { line =>
      val values = line.split(",")
      val Array(minX, maxX, minY, maxY) = values
      val pairs = (minX.toInt - lastX, maxX.toInt - minX.toInt)
      lastX = maxX.toInt
      pairs
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

  def main(args:Array[String]) {
    val caseNames = expandCaseNames(args)
    println(caseNames)
    caseNames.foreach { caseName =>
      val widthPairs = readWidthPairs(caseName)
      val widths = widthPairs.map { _._2 + 1 }.toList.filter { _ > 4 }.sorted
      println((caseName, widths(widths.size * 3 / 4)))
    }

/*
    val caseNameToSymbols = readAnnotations()
    easyCaseNames.foreach { caseName =>
      val symbols = caseNameToSymbols(caseName).toList
      println(symbols.toList)
      println(findWarnings(symbols.toList, Context.startingContext))
    }
*/
    System.exit(0)
  }
}
