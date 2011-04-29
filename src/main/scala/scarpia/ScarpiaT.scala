package scarpia
import errors._
import Pos._

import scalaz._
import Scalaz._

import Parsers._

/** The Scarpia parser monad transformer. */
abstract class ScarpiaT[S, U, M[_], A](implicit m: Monad[M]) {
  val scarpiaModule = new ScarpiaTs[S, U, M]
  import scarpiaModule._

  def apply[B](s: ParserState[S, U],
               cok: => (=> A, ParserState[S, U], => ParseError) => M[B], // Consumed-OK
               cerr: => (=> ParseError) => M[B],                         // Consumed-error
               eok: => (=> A, ParserState[S, U], => ParseError) => M[B], // Empty-OK
               eerr: => (=> ParseError) => M[B]): M[B]                   // Empty-error

  /** Low-level unpacking of the ScarpiaT type. To run your parser, use runPT, runP, or parse. */
  def run(s: ParserState[S, U]): M[Consumed[M[Reply[S, U, A]]]] = {
    def cok(a: => A, sp: ParserState[S, U], err: => ParseError) = 
      Consumed(Ok(a, sp, err).pure[M]).pure[M]
    def cerr(err: => ParseError) =
      Consumed(Error[S, U, A](err).pure[M]).pure[M]
    def eok(a: => A, sp: ParserState[S, U], err: => ParseError) =
      Empty(Ok(a, sp, err).pure[M]).pure[M]
    def eerr(err: => ParseError) = 
      Empty(Error[S, U, A](err).pure[M]).pure[M]
    apply(s, cok, cerr, eok, eerr)
  }

  /** Run the parser in a monad. */
  def runPT(u: U, name: SourceName, s: S): M[Either[ParseError, A]] = {
    def parserReply(res: Consumed[M[Reply[S, U, A]]]) = res match {
      case Consumed(r) => r
      case Empty(r) => r
    }
    for {
      res <- run(ParserState(s, initialPos(name), u))
      r <- parserReply(res)
    } yield r match {
      case Ok(x, _, _) => Right(x)
      case Error(err) => Left(err)
    }
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {
    def apply[C](s: ParserState[S, U],
                 cok: => (=> B, ParserState[S, U], => ParseError) => M[C],
                 cerr: => (=> ParseError) => M[C],
                 eok: => (=> B, ParserState[S, U], => ParseError) => M[C],
                 eerr: => (=> ParseError) => M[C]): M[C] = {
      def mcok(x: => A, s: ParserState[S, U], err: => ParseError) = 
        f(x)(s, cok, cerr, cok, e => cerr(err merge e))
      def meok(x: => A, s: ParserState[S, U], err: => ParseError) =
        f(x)(s, cok, cerr, eok, e => eerr(err merge e))
      ScarpiaT.this.apply(s, mcok, cerr, meok, eerr)
    }
  }

  def map[B](f: A => B): Parser[B] = flatMap(x => ScarpiaTMonad[S, U, M].pure(f(x)))

  /**
   * An alias for label. Replaces the "expected" error messages with the given message, if this parser fails.
   */ 
  def ?(msg: String) = label(msg)

  /**
   * Tries this parser and pretends that it hasn't consumed any input when an error occurs.
   * Used whenever arbitrary lookahead is needed.
   */
  def attempt: Parser[A] = new Parser[A] {
    def apply[B](s: ParserState[S, U],
                 cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 cerr: => (=> ParseError) => M[B],
                 eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 eerr: => (=> ParseError) => M[B]): M[B] = s match {
      case ParserState(_, pos, _) => {
        def pcerr(parseError: => ParseError) = eerr(parseError setErrorPos pos)
        ScarpiaT.this.apply(s, cok, pcerr, eok, eerr)
      }
    }
  }

  /**
   * Replaces the "expected" error messages with the given message, if this parser fails.
   */ 
  def label(msg: String) = labels(List(msg)) 

  /**
   * Replaces the "expected" error messages with the given messages, if this parser fails.
   */ 
  def labels(msgs: List[String]) = new ScarpiaT[S, U, M, A] {
    def apply[B](s: ParserState[S, U],
               cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
               cerr: => (=> ParseError) => M[B],
               eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
               eerr: => (=> ParseError) => M[B]): M[B] = {
                 def eokp(x: => A, sp: ParserState[S, U], error: => ParseError) =
                   eok(x, sp, if (error.isUnknown) error else error.setExpectErrors(msgs))
                 def eerrp(err: => ParseError) = eerr(err.setExpectErrors(msgs))
                 ScarpiaT.this.apply(s, cok, cerr, eokp, eerrp)
             }
  }

  /**
   * Choice combinator. P | Q first applies P. If it succeeds, the value of P is returned. If
   * P fails without consuming any input, parser Q is tried.
   */
  def |(q: Parser[A]): Parser[A] = this <+> q

  /** P.many applies P zero or more times. */
  def many: Parser[Stream[A]] = manyAccum(_ #:: _) map (_.reverse)

  def manyAccum(acc: (A, Stream[A]) => Stream[A]): Parser[Stream[A]] = {
    val p = this
    new Parser[Stream[A]] {
      def apply[B](s: ParserState[S, U],
                   cok: => (=> Stream[A], ParserState[S, U], => ParseError) => M[B],
                   cerr: => (=> ParseError) => M[B],
                   eok: => (=> Stream[A], ParserState[S, U], => ParseError) => M[B],
                   eerr: => (=> ParseError) => M[B]): M[B] = {
        def walk(xs: => Stream[A], x: => A, sp: ParserState[S, U], err: => ParseError): M[B] = {
          val seqxs = xs
          p(sp, walk(acc(x, seqxs), _, _, _), cerr, manyErr, e => cok(acc(x, xs), sp, e))
        }
        p(s, walk(Stream(), _, _, _), cerr, manyErr, e => eok(Stream(), s, e))
      }
    }
  }

  def manyErr = error("Combinator * is applied to a parser that accepts an empty string.")

  def unary_![T](implicit s: Uncons[M, S, T], sa: Show[A], m: Monad[M]): Parser[Unit] = ((for {
    c <- attempt
    _ <- unexpected[T, A](c.shows)
  } yield ()) | ScarpiaTMonad[S, U, M].pure(())).attempt

  /** P sepBy Q parses zero or more occurrences of P, separated by Q. */
  def sepBy[T, Sep](sep: Parser[Sep])(implicit s: Uncons[M, S, T]): Parser[Stream[A]] =
    sepBy1(sep) | Stream[A]().pure[Parser]

  /** P sepBy Q parses one or more occurrences of P, separated by Q. */
  def sepBy1[T, Sep](sep: Parser[Sep])(implicit s: Uncons[M, S, T]) =
    (this |@| (sep *> this).many)(_ #:: _)

  /** P endBy Q parses zero or more occurrences of P, separated and ended by Q. */
  def endBy[T, Sep](sep: Parser[Sep])(implicit s: Uncons[M, S, T]) =
    (this <* sep) many
}

/** A module of parsers with input type S, user state U, with output in monad M. */ 
class ScarpiaTs[S, U, M[_]: Monad] {
  type Parser[A] = ScarpiaT[S, U, M, A]

  /** Parses a single character. */
  def char(c: Char)(implicit u: Uncons[M, S, Char]): Parser[Char] =
    satisfy(_ == c) ? List(c).shows

  // Converts a character to a parser which matches that character.
  implicit def charToParser(c: Char)(implicit u: Uncons[M, S, Char]): Parser[Char] = char(c)

  /**
   * Succeeds for any character for which the supplied function returns true.
   * Returns the character that is actually parsed.
   */
  def satisfy(p: Char => Boolean)(implicit u: Uncons[M, S, Char]): Parser[Char] =
    tokenPrim((c: Char) => List(c).shows, (pos, c: Char, cs) => pos updatePosChar c, (c: Char) => if (p(c)) some(c) else none)

  /** Succeeds for any character which is not contained by the given string. */
  def noneOf(s: String)(implicit u: Uncons[M, S, Char]): Parser[Char] =
    satisfy(c => !s.toList.contains(c))

  /** A parser that only succeeds at the end of the input. */
  def eof[T](implicit sm: Uncons[M, S, T], s: Show[T]): Parser[Unit] =
    (!anyToken) ? "end of input"

  /** A parser that accepts any kind of token. Returns the accepted token. */
  def anyToken[T](implicit sm: Uncons[M, S, T], s: Show[T]): Parser[T] =
    tokenPrim[T, T]((t: T) => t.shows, (pos, tok, toks) => pos, some)

  /** Primitive combinator for accepting tokens that can be represented as strings. */
  def tokenPrim[T, A](showToken: T => String,
                      nextPos: (SourcePos, T, S) => SourcePos,
                      test: T => Option[A])
                     (implicit sm: Uncons[M, S, T]): Parser[A] =
    tokenPrimEx(showToken, nextPos, none, test)

  def tokenPrimEx[T, A](showToken: T => String,
                        nextPos: (SourcePos, T, S) => SourcePos,
                        nextState: Option[(SourcePos, T, S, U) => U],
                        test: T => Option[A])
                       (implicit sm: Uncons[M, S, T]): Parser[A] =
    new Parser[A] {
      def apply[B](s: ParserState[S, U],
                   cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   cerr: => (=> ParseError) => M[B],
                   eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   eerr: => (=> ParseError) => M[B]): M[B] = s match {
        case ParserState(input, pos, user) => for {
          r <- sm.uncons(input)
          result <- r match {
            case None => eerr(unexpectError("", pos))
            case Some((c, cs)) => test(c) match {
              case Some(x) => {
                val newpos = nextPos(pos, c, cs)
                val newstate = ParserState(cs, newpos, user)
                cok(x, newstate, newpos.unknownError)
              }
              case None => eerr(unexpectError(showToken(c), pos))
            }
          }
        } yield result
      }
    }

  /** Always fails with an unexpected error message without consuming any input. */
  def unexpected[T, A](msg: String)(implicit str: Uncons[M, S, T]): Parser[A] = new Parser[A] {
    def apply[B](s: ParserState[S, U],
                 cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 cerr: => (=> ParseError) => M[B],
                 eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 eerr: => (=> ParseError) => M[B]): M[B] = {
      eerr(errorMessage(UnExpect(msg), s.pos))
    }
  }

  implicit def parserMA[A](p: Parser[A]): MA[Parser, A] = ma[Parser, A](p)

  implicit def parserPlus: Plus[Parser] = new Plus[Parser] {
    def plus[A](m: Parser[A], n: => Parser[A]): Parser[A] =
      new Parser[A] {
        def apply[B](s: ParserState[S, U],
                     cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                     cerr: => (=> ParseError) => M[B],
                     eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                     eerr: => (=> ParseError) => M[B]): M[B] = {
                       def meerr(err: => ParseError) = {
                         def neok(y: => A, sp: ParserState[S, U], errp: => ParseError) = eok(y, sp, err merge errp)
                         def neerr(errp: => ParseError) = eerr(err merge errp)
                           n(s, cok, cerr, neok, neerr)
                       }
                       m(s, cok, cerr, eok, meerr)
                     }
      }
  }

  /** A parser that always fails without consuming any input */
  implicit def parserZero[A]: Zero[Parser[A]] = new Zero[Parser[A]] {
    val zero = new Parser[A] {
        def apply[B](s: ParserState[S, U],
                     cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                     cerr: => (=> ParseError) => M[B],
                     eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                     eerr: => (=> ParseError) => M[B]): M[B] = eerr(s.unknownError)
    }
  }

  def mkST[A](k: ParserState[S, U] => M[Consumed[M[Reply[S, U, A]]]]): Parser[A] =
    new Parser[A] {
      def apply[B](s: ParserState[S, U],
                   cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   cerr: => (=> ParseError) => M[B],
                   eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                   eerr: => (=> ParseError) => M[B]): M[B] = for {
                     cons <- k(s)
                     r    <- cons match {
                       case Consumed(mrep) => for {
                         rep <- mrep
                         r   <- rep match {
                           case Ok(x, sp, err) => cok(x, sp, err)
                           case Error(err) => cerr(err)
                         }
                       } yield r
                       case Empty(mrep) => for {
                         rep <- mrep
                         r   <- rep match {
                           case Ok(x, sp, err) => eok(x, sp, err)
                           case Error(err) => eerr(err)
                         }
                       } yield r
                     }
                 } yield r
    }

  /** Monadic parser is monadic. */
  implicit val parserMonad: Monad[Parser] = ScarpiaTMonad[S, U, M]

  /** A parser that always fails with the given raw message without consuming input. */
  def parserFail[A](msg: String): Parser[A] = new ScarpiaT[S, U, M, A] {
    def apply[B](s: ParserState[S, U],
                 cok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 cerr: => (=> ParseError) => M[B],
                 eok: => (=> A, ParserState[S, U], => ParseError) => M[B],
                 eerr: => (=> ParseError) => M[B]): M[B] =
               eerr(errorMessage(RawMessage(msg), s.pos))
  }
}
