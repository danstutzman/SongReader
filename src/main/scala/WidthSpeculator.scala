import scala.collection.immutable.TreeMap

trait Chance {}

case class Context(
  val hadClef:Boolean,
  val had44TimeSig:Boolean,
  val beatsSoFar:Float
) {}

object Context {
  val startingContext = Context(false, false, 0.0f)
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

  def breakDownChances(symbols:List[String], context:Context) : List[Chance] = {
    symbols match {
      case Nil => Nil
      case symbol :: otherSymbols =>
        breakDownChances(otherSymbols, context)
    }
  }

  def main(args:Array[String]) {
    val caseNameToSymbols = readAnnotations()
    easyCaseNames.foreach { caseName =>
      val symbols = caseNameToSymbols(caseName).toList
      println(symbols.toList)
      println(breakDownChances(symbols.toList, Context.startingContext))
    }
    System.exit(0)
  }
}
