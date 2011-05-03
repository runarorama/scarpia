import scalaz._
import Scalaz._
import effects._
import scarpia._
import scarpia.errors._
import Parsers._

object CSV {
  val gp = new GenParsers[Char, Unit]
  import gp._

  val eol = ("\n\r".attempt | "\r\n".attempt | "\n" | "\r") ? "end of line"

  val quotedChar = noneOf("\"") | (string("\"\"") >|> '"'.pure[Parser]).attempt

  val quotedCell = for {
    _ <- char('"')
    content <- quotedChar many
    _ <- char('"') ? "quote at end of cell"
  } yield content

  def parseCSV(input: Stream[Char]): Either[ParseError, Stream[List[String]]] = 
    parse(csvFile, "unknown", input)

  def mainIO = for {
    c <- getContents
    x <- parse(csvFile, "(stdin)", c) match {
      case Left(e) => for {
        _ <- putStrLn("Error parsing input:")
        _ <- putOut(e)
      } yield ()
      case Right(r) => r traverse_ putOut
    }
  } yield x

  def main(args: Array[String]) = mainIO.unsafePerformIO
}
