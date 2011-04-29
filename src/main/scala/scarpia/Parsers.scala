package scarpia
import scalaz._
import Scalaz._

object Parsers {
  import errors._
  import Pos._

  def unexpectError(msg: String, pos: SourcePos) = errorMessage(SysUnExpect(msg), pos)

  abstract class Uncons[M[_], S, T](implicit monad: Monad[M]) {
    def uncons(s: S): M[Option[(T, S)]]
  }

  implicit def unconsStream[M[_]: Monad, T]: Uncons[M, Stream[T], T] = new Uncons[M, Stream[T], T] {
    def uncons(s: Stream[T]) = s match {
      case Stream() => none.pure[M]
      case (h #:: t) => some((h, t)).pure[M]
    }
  }

  type Scarpia[S, U, A] = ScarpiaT[S, U, Identity, A]

  // String parsers
  type GenParser[T, S, A] = Scarpia[Stream[T], S, A]
  type StringParser[A] = Scarpia[String, Unit, A]

  def ScarpiaTFunctor[S, U, M[_]: Functor] = new Functor[ScarpiaTs[S, U, M]#Parser] {
    def fmap[A, B](m: ScarpiaT[S, U, M, A], f: A => B): ScarpiaT[S, U, M, B] = m map f
  }

  def ScarpiaTMonad[S, U, M[_]: Monad] = new Monad[ScarpiaTs[S, U, M]#Parser] {
    def pure[A](a: => A) = new ScarpiaT[S, U, M, A] {
      def apply[B](s: ParserState[S, U],
                   cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   cerr: => (=> ParseError) => M[B],
                   eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   eerr: => (=> ParseError) => M[B]): M[B] =
                 eok(a, s, s.unknownError)
    }
    def bind[A, B](p: ScarpiaT[S, U, M, A], f: A => ScarpiaT[S, U, M, B]) = p flatMap f
  } 

  implicit val consumedFunctor = new Functor[Consumed] {
    def fmap[A, B](c: Consumed[A], f: A => B): Consumed[B] = c match {
      case Consumed(x) => Consumed(f(x))
      case Empty(x) => Consumed(f(x))
    }
  }

  implicit def ReplyFunctor[S, U] = new Functor[PartialApply2Of3[Reply, S, U]#Apply] {
    def fmap[A, B](r: Reply[S, U, A], f: A => B): Reply[S, U, B] = r match {
      case Ok(x, s, e) => Ok(f(x), s, e)
      case Error(e) => Error(e)
    }
  }

  def errorMessage(msg: Message, pos: SourcePos) = ParseError(pos, List(msg))

  implicit val parseErrorShow: Show[ParseError] = new Show[ParseError] {
    def show(e: ParseError) = (e.pos + ":" +
      showErrorMessages("or", "unknown parse error", "expecting", "unexpected", "end of input", e.messages)).toList
  }

  def showErrorMessages(or: String, unknown: String, expecting: String,
                        unexpected: String, endOfInput: String, msgs: List[Message]) = {
    val (sysUnexpect, msgs1): (List[Message], List[Message]) = msgs.span(SysUnExpect("") == _)
    val (unExpect, msgs2) = msgs1.span(UnExpect("") == _)
    val (expect, messages) = msgs2.span(Expect("") == _)
    def clean(xs: List[String]) = xs.filterNot(_ == "").distinct
    def commaSep(msgs: List[String]) = clean(msgs).mkString(",")
    val commasOr: List[String] => String = {
      case Nil => ""
      case m :: Nil => m
      case ms => commaSep(ms.init) + " " + or + " " + ms.last
    }
    def showMany(pre: String, msgs: List[Message]) = clean(msgs.map(_.string)) match {
      case Nil => ""
      case ms => if (pre.isEmpty) commasOr(ms) else
                    pre + " " + commasOr(ms)
    }
    val showExpect = showMany(expecting, expect)
    val showUnExpect = showMany(unexpected, unExpect)
    val showSysUnexpect = {
      val firstMsg = sysUnexpect.head.string
      if (unExpect.isEmpty || sysUnexpect.isEmpty) "" else
      if (firstMsg.isEmpty) unexpected + " " + endOfInput else
         unexpected + " " + firstMsg
    }
    val showMessages = showMany("", messages) 
    if (msgs.isEmpty) unknown else clean(
      List(showSysUnexpect, showUnExpect, showExpect, showMessages)).flatMap("\n" + _)
  }
}

