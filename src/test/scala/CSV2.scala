import scalaz._
import Scalaz._
import scarpia._

object CSV2 {
  val parser = new scarpia.GenParsers[Char, Unit]
  import parser._

  val csvFile = line endBy '\n'
  val line = cell sepBy ','
  val cell = noneOf(",\n") many
}

