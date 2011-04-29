package scarpia
package errors

import scalaz._
import Scalaz._

import Pos._

sealed abstract class Message(val level: Int) {
  def string: String
}

case class SysUnExpect(string: String) extends Message(0)
case class UnExpect(string: String) extends Message(1)
case class Expect(string: String) extends Message(2)
case class RawMessage(string: String) extends Message(3)

sealed trait ParseError {
  val pos: SourcePos
  val messages: List[Message]
  def isUnknown = messages.isEmpty
  def addErrorMessage(msg: Message) = ParseError(pos, msg :: messages)
  def setErrorPos(newPos: SourcePos) = ParseError(newPos, messages)
  def setErrorMessage(msg: Message) = ParseError(pos, msg :: messages.filterNot(_ == msg))
  def merge(p: ParseError): ParseError = ParseError(pos, messages ++ p.messages)
  def setExpectErrors: List[String] => ParseError = {
    case Nil => setErrorMessage(Expect(""))
    case x :: Nil => setErrorMessage(Expect(x))
    case x :: xs => xs.foldr(setErrorMessage(Expect(x)))((msg, err) => err.addErrorMessage(Expect(msg)))
  }
}

object ParseError {
  def apply(p: SourcePos, m: List[Message]) = new ParseError {
    val pos = p
    val messages = m
  }
  def unapply(p: ParseError) = some((p.pos, p.messages))
} 


