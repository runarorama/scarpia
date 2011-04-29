import scalaz._
import Scalaz._
import scarpia._

object CSV2 {
  val parser = new GenParsers[Char, Unit]
  import parser._

  val cell = (noneOf(",\n") many) map (_.mkString)
  val line = cell sepBy ','
  val csvFile: Parser[Stream[Stream[String]]] = line endBy '\n'

  def parseCSV(input: Stream[Char]) =
    parse(csvFile, "unknown", input)
}

