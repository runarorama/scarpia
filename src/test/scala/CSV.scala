
object CSV {
  import scalaz._
  import Scalaz._
  import scarpia._
  import scarpia.errors._

  val gp = new GenParsers[Char, Unit]
  import gp._

  val csvFile: Parser[Stream[List[String]]] = for {
    result <- line.many
    _ <- eof
  } yield result

  val line: Parser[List[String]] = for {
    result <- cells
    _ <- eol
  } yield result

  val cells: Parser[List[String]] = for {
    first <- cellContent
    next <- remainingCells
  } yield first :: next

  val remainingCells: Parser[List[String]] =
    (char(',') >|> cells) | List[String]().pure[({type l[x] = GenParser[Char, Unit, x]})#l]

  val cellContent: Parser[String] = 
    noneOf(",\n").many map (_.mkString)

  val eol: Parser[Char] = 
    char('\n')

  def parseCSV(input: Stream[Char]): Either[ParseError, Stream[List[String]]] = 
    parse(csvFile, "unknown", input)
}
