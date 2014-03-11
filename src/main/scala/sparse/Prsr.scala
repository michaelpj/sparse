package sparse

import scala.util.parsing.combinator
import scala.util.parsing.combinator.RegexParsers
import scalaz.std.util.parsing.combinator.parser._
import scalaz._
import scalaz.syntax.applicativePlus._
import scalaz.std.option._
import scalaz.syntax.monoid._

import scala.language.existentials

object OptionParsers extends RegexParsers {
  case class OptParams[T](names: List[String] = List(), metavar: String = "", default: Option[T] = None)
  implicit def paramFunctor: Functor[OptParams] = new Functor[OptParams] {
    def map[A,B](fa: OptParams[A])(f: A => B): OptParams[B] = fa.copy(default = fa.default.map(f))
  }
  
  sealed trait OptReader[T]
  case class OptionReader[T](name: String, p: Parser[T]) extends OptReader[T]
  
  case class Opt[T](params: OptParams[T], parser: Parser[T]) {
    def matches(s: String): Boolean = params.names.exists(s == "--" + _)
  }
  
  implicit def optionFunctor: Functor[Opt] = new Functor[Opt] {
    def map[T,U](fa: Opt[T])(f: T => U): Opt[U] = Opt(fa.params.map(f), fa.parser.map(f))
  }
  
  sealed trait Prsr[+T]
  case class NilP[T](t: Option[T]) extends Prsr[T]
  trait ConsP[T] extends Prsr[T] {
    type U
    val opt: Prsr[U=>T]
    val rest: Prsr[U]
  }
  object ConsP {
    def apply[T,U2](opt2: Prsr[U2=>T], rest2: Prsr[U2]) = new ConsP[T] {
      type U = U2
      val opt: Prsr[U => T] = opt2
      val rest: Prsr[U] = rest2
    }
    def unapply[T](c: ConsP[T]): Option[(Prsr[U=>T], Prsr[U])] forSome { type U } = Some((c.opt, c.rest))
  }
  case class OptP[T](opt: Opt[T]) extends Prsr[T]
  case class AltP[T](left: Prsr[T], right: Prsr[T]) extends Prsr[T]
  
  implicit def parserFunctor: Functor[Prsr] = new Functor[Prsr] {
    def map[T, U](fa: Prsr[T])(f: T => U): Prsr[U] = fa match {
      case NilP(o) => NilP(o.map(f))
      case OptP(opt) => OptP(opt.map(f))
      case ConsP(opt, rest) => ConsP(opt.map(f.compose(_)), rest)
      case AltP(left, right) => AltP(left.map(f), right.map(f))
    }
  }
  
  def toOption[T](res: ParseResult[T]): Option[T] = res match {
    case Success(v, _) => Some(v)
    case NoSuccess(_, _) => None
  }
  
  implicit def parserApplicative: ApplicativePlus[Prsr] = new ApplicativePlus[Prsr] {
    def point[T](t: => T): Prsr[T] = NilP(Some(t))
    def ap[A,B](fa: => Prsr[A])(f: => Prsr[A => B]): Prsr[B] = ConsP(f, fa)
    def plus[A](a: Prsr[A], b: => Prsr[A]): Prsr[A] = AltP(a, b)
    def empty[A]: Prsr[A] = NilP(None)
  }

  def option[T](parser: Parser[T], m: Mod[T]): Prsr[T] = OptP(Opt(m(OptParams()), parser))
  def flag[T](parser: Parser[T], default: T): Prsr[T] = active.point[Prsr] <+> default.point[Prsr]
  
  def optionSearch[T](o: Opt[T], args: List[String]): Option[(T, List[String])] = args match {
    case name :: value :: tail => 
      if (o.matches(name)) toOption(parseAll(o.parser, value)).map((_, tail)) 
      else optionSearch(o, tail).map { case (t, as) => (t, name :: value :: as) }
    case _ => None
  }
  
  def parse[T](p: Prsr[T], args: List[String]): Option[(T, List[String])] = p match {
    case NilP(o) => o.map((_,args))
    case OptP(opt) => optionSearch(opt, args)
    case ConsP(opt, rest) => for {
      (f, rem) <- parse(opt, args)
      (v, rem2) <- parse(rest, rem)
    } yield (f(v), rem2)
    case AltP(left, right) => parse(left, args) <+> parse(right, args)
  }
  
  def evalParser[T](p: Prsr[T]): Option[T] = p match {
    case NilP(o) => o
    case OptP(_) => None
    case ConsP(opt, rest) => evalParser(rest) <*> evalParser(opt)
    case AltP(left, right) => evalParser(left) <+> evalParser(right)
  }
  
  trait Mod[T] {
    val underlying: OptParams[T]=>OptParams[T]
    def apply(o: OptParams[T]) = underlying(o)
  }
  
  object Mod {
    def apply[T](f: OptParams[T] => OptParams[T]) = new Mod[T] { val underlying = f }
  }
  
  implicit def modMonoid[T]: Monoid[Mod[T]] = new Monoid[Mod[T]] {
    def zero: Mod[T] = Mod(identity)
    def append(a: Mod[T], b: => Mod[T]): Mod[T] = Mod(a.underlying compose b.underlying)
  }
  
  def long[T](name: String): Mod[T] = Mod(ps => ps.copy(names = name :: ps.names))
  def short[T](name: Char): Mod[T] = Mod(ps => ps.copy(names = name.toString :: ps.names))
  def metavar[T](meta: String): Mod[T] = Mod(_.copy(metavar = meta))
  def default[T](value: T): Mod[T] = Mod(_.copy(default = Some(value)))
  
  case class User(name: String, id: Int)
  
  val testParser: Prsr[User] = (
      option(".*".r, 
          long[String]("name") |+| short('n') |+| metavar("NME")) 
      |@| option("\\d+".r ^^ { _.toInt }, 
          long("id"))
      ) { User.apply }
}
