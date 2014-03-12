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
  sealed trait Name
  case class Long(n: String) extends Name
  case class Short(c: Char) extends Name
  
  case class OptParams[+T](metavar: String = "", help: String = "", default: Option[T] = None)
  implicit def paramFunctor: Functor[OptParams] = new Functor[OptParams] {
    def map[A,B](fa: OptParams[A])(f: A => B): OptParams[B] = fa.copy(default = fa.default.map(f))
  }
  
  sealed trait OptReader[T]
  case class OptionReader[T](name: String, p: Parser[T]) extends OptReader[T]
  
  case class Opt[T](name: Name, params: OptParams[T], parser: Parser[T]) {
    def matches(s: String): Boolean = name match {
      case Long(n) => s == "--" + n
      case Short(c) => s == "-" + c
    }
  }
  
  implicit def optionFunctor: Functor[Opt] = new Functor[Opt] {
    def map[T,U](fa: Opt[T])(f: T => U): Opt[U] = Opt(fa.name, fa.params.map(f), fa.parser.map(f))
  }
  
  sealed trait Prsr[+T] {
    def mod[U>:T](m: ModP[U]): Prsr[U] = m(this)
  }
  case class Done[T](t: Option[T]) extends Prsr[T]
  trait Chain[T] extends Prsr[T] {
    type U
    val rest: Prsr[U=>T]
    val opt: Prsr[U]
  }
  object Chain {
    def apply[T,U2](opt2: Prsr[U2], rest2: Prsr[U2=>T]) = new Chain[T] {
      type U = U2
      val rest: Prsr[U => T] = rest2
      val opt: Prsr[U] = opt2
    }
    def unapply[T](c: Chain[T]): Option[(Prsr[U], Prsr[U=>T])] forSome { type U } = Some((c.opt, c.rest))
  }
  case class OptP[T](opt: Opt[T]) extends Prsr[T]
  case class Alt[T](left: Prsr[T], right: Prsr[T]) extends Prsr[T]
  
  def toOption[T](res: ParseResult[T]): Option[T] = res match {
    case Success(v, _) => Some(v)
    case NoSuccess(_, _) => None
  }
  
  implicit def parserApplicative: ApplicativePlus[Prsr] = new ApplicativePlus[Prsr] {
    // more efficient map
    override def map[T, U](fa: Prsr[T])(f: T => U): Prsr[U] = fa match {
      case Done(o) => Done(o.map(f))
      case OptP(opt) => OptP(opt.map(f))
      case Chain(opt, rest) => Chain(opt, rest.map(f.compose(_)))
      case Alt(left, right) => left.map(f) <+> right.map(f)
    }
    def point[T](t: => T): Prsr[T] = Done(Some(t))
    def ap[A,B](fa: => Prsr[A])(f: => Prsr[A => B]): Prsr[B] = Chain(fa, f)
    def plus[A](a: Prsr[A], b: => Prsr[A]): Prsr[A] = Alt(a, b)
    def empty[A]: Prsr[A] = Done(None)
  }

  def option[T](name: String, p: Parser[T]): Prsr[T] = OptP(Opt(Long(name), OptParams(), p))
  def strOpt(name: String): Prsr[String] = option(name, """.*""".r)
  def intOpt(name: String): Prsr[Int] = option(name, """\d+""".r ^^ {_.toInt})
  
  //def flag[T](parser: Parser[T], default: T): Prsr[T] = active.point[Prsr] <+> default.point[Prsr]
  
  def optionSearch[T](o: Opt[T], args: List[String]): Option[(T, List[String])] = args match {
    case name :: value :: tail => 
      if (o.matches(name)) toOption(parseAll(o.parser, value)).map((_, tail)) 
      else optionSearch(o, tail).map { case (t, as) => (t, name :: value :: as) }
    case _ => None
  }
  
  def parse[T](p: Prsr[T], args: List[String]): Option[(T, List[String])] = p match {
    case Done(o) => o.map((_,args))
    case OptP(opt) => optionSearch(opt, args)
    case Chain(opt, rest) => for {
      (v, rem) <- parse(opt, args)
      (f, rem2) <- parse(rest, rem)
    } yield (f(v), rem2)
    case Alt(left, right) => parse(left, args) <+> parse(right, args)
  }
  
  def evalParser[T](p: Prsr[T]): Option[T] = p match {
    case Done(o) => o
    case OptP(_) => None
    case Chain(opt, rest) => evalParser(opt) <*> evalParser(rest)
    case Alt(left, right) => evalParser(left) <+> evalParser(right)
  }
  
  def updateParams[T](f: OptParams[T]=>OptParams[T], p: Prsr[T]): Prsr[T] = p match {
    case OptP(o) => OptP(o.copy(params = f(o.params)))
    case Alt(left, right) => updateParams(f, left) <+> updateParams(f, right)
    case o => o
  }
  
  trait Mod[T] {
    val underlying: T=>T
    def apply(o: T) = underlying(o)
  }
  
  object Mod {
    def apply[T](f: T => T) = new Mod[T] { val underlying = f }
    def optMod[T](f: OptParams[T]=>OptParams[T]): ModP[T] = Mod(updateParams(f, _))
  }
  
  type ModP[T] = Mod[Prsr[T]]
  
  implicit def modMonoid[T]: Monoid[Mod[T]] = new Monoid[Mod[T]] {
    def zero: Mod[T] = Mod(identity)
    def append(a: Mod[T], b: => Mod[T]): Mod[T] = Mod(a.underlying compose b.underlying)
  }
  
  def long[T](n: String): ModP[T] = Mod {
    case opt@OptP(o) => Alt(opt, OptP(o.copy(name=Long(n))))
    case o => o
  }
  def short[T](n: Char): ModP[T] = Mod {
    case opt@OptP(o) => Alt(opt, OptP(o.copy(name=Short(n))))
    case o => o
  }
  def metavar[T](meta: String): ModP[T] = Mod.optMod(_.copy(metavar=meta))
  def default[T](value: T): ModP[T] = Mod.optMod(_.copy(default=Some(value)))
  def help[T](value: String): ModP[T] = Mod.optMod(_.copy(help=value))
  
  case class User(name: String, id: Int)
  
  val testParser: Prsr[User] = (
      strOpt("name").mod(short[String]('n') |+| metavar("NME") |+| help("The name of the user")) 
      |@| intOpt("id").mod(help("The id of the user"))
      ) { User.apply }
}
