package scarpia

import scalaz._
import Scalaz._

import scarpia.errors._
import Pos._
import Parsers._

/** A module of pure parsers with input type S and user state U. */
class Scarpias[S, U] extends ScarpiaTs[S, U, Identity] {
  def parse[S, A](p: Scarpia[S, Unit, A], name: SourceName, s: S): Either[ParseError, A] =
    runP(p, (), name, s)
  def runP[S, U, T, A](p: Scarpia[S, U, A], u: U, name: SourceName, s: S): Either[ParseError, A] =
    p.runPT(u, name, s).value
}

/** A module of parsers that consume a token stream. */
class GenParsers[T, U] extends Scarpias[Stream[T], U]

/** A module of parsers whose input are strings. */
class StringParsers extends Scarpias[String, Unit]

/** A module of parsers whose input are streams of characters */
class CharParser extends GenParsers[Char, Unit]

trait Consumed[A] {
  def apply[B](consumed: (=> A) => B, empty: A => B): B
}

object Consumed {
  def apply[A](a: => A): Consumed[A] = new Consumed[A] {
    lazy val va = a
    def apply[B](consumed: (=> A) => B, empty: A => B): B = consumed(va)
  }
  def unapply[A](c: Consumed[A]): Option[A] = c(consumed = a => some(a), empty = a => none)
}

object Empty {
  def apply[A](a: => A): Consumed[A] = new Consumed[A] {
    lazy val va = a
    def apply[B](consumed: (=> A) => B, empty: A => B): B = empty(va)
  }
  def unapply[A](c: Consumed[A]): Option[A] = c(consumed = a => none, empty = a => some(a))
}

trait Reply[S, U, A] {
  def apply[B](ok: (=> A, ParserState[S, U], => ParseError) => B, error: (=> ParseError) => B): B
  def mergeError(err1: ParseError) = this match {
    case Ok(x, state, err2) => Ok(x, state, err1 merge err2)
    case Error(err2) => Error(err1 merge err2)
  }
}

object Ok {
  def apply[S, U, A](a: => A, s: ParserState[S, U], err: => ParseError): Reply[S, U, A] = new Reply[S, U, A] {
    lazy val va = a
    lazy val verr = err
    def apply[B](ok: (=> A, ParserState[S, U], => ParseError) => B, error: (=> ParseError) => B): B =
      ok(va, s, verr)
  }
  def unapply[S, U, A](r: Reply[S, U, A]): Option[(A, ParserState[S, U], ParseError)] =
    r(ok = (a, u, e) => some((a, u, e)), error = e => none)
}

object Error {
  def apply[S, U, A](err: => ParseError): Reply[S, U, A] = new Reply[S, U, A] {
    lazy val verr = err
    def apply[B](ok: (=> A, ParserState[S, U], => ParseError) => B, error: (=> ParseError) => B): B =
      error(verr)
  }
  def unapply[S, U, A](r: Reply[S, U, A]): Option[ParseError] = r(ok = (a, u, e) => none, e => some(e))
}

trait ParserState[S, U] {
  def unknownError[S, U]: ParseError = pos.unknownError
  val input: S
  val pos: SourcePos
  val user: U
}

object ParserState {
  def apply[S, U](si: => S, sp: SourcePos, su: U): ParserState[S, U] = new ParserState[S, U] {
    lazy val input = si
    val pos = sp
    val user = su
  }
  def unapply[S, U](ps: ParserState[S, U]): Option[(S, SourcePos, U)] =
    some((ps.input, ps.pos, ps.user))
}
