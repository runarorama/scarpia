package scarpia
import errors._

import scalaz._
import Scalaz._

object Pos {

  // Source positions are determined by file name, line, column
  type SourceName = String
  type Line = Int
  type Column = Int

  sealed class SourcePos(val name: SourceName, val line: Line, val column: Column) {
    def unknownError = ParseError(this, List())
    def setSourceName(s: String) = new SourcePos(s, line, column)
    def setSourceLine(l: Line) = new SourcePos(name, l, column)
    def incSourceLine(n: Int) = new SourcePos(name, line + n, column)
    def incSourceColumn(n: Int) = new SourcePos(name, line, column + n)
    def updatePos(s: String): SourcePos = s.toStream.foldl(this)(_.updatePosChar(_))
    def updatePosChar(c: Char): SourcePos = c match {
      case '\n' => new SourcePos(name, line + 1, 1)
      case '\t' => new SourcePos(name, line, column + 8 - ((column - 1) % 8))
      case _ => new SourcePos(name, line, column + 1)
    }
  }

  implicit val sourcePosShow: Show[SourcePos] = shows(p => {
    def showLineColumn = "(line " + p.line.shows + ", column " + p.column.shows + ")"
    if (p.name == "") showLineColumn else "\"" + p.name + "\" " + showLineColumn
  })

  def initialPos(name: SourceName) = new SourcePos(name, 1, 1)
}
