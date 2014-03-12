package sparse

import scala.util.parsing.combinator
import scala.util.parsing.combinator.RegexParsers
import scalaz.std.util.parsing.combinator.parser._
import scalaz._
import scalaz.syntax.applicativePlus._
import scalaz.std.option._
import scalaz.syntax.monoid._
import scalaz.syntax.id._
import scala.language.existentials
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ListReader[T](l: List[T]) extends input.Reader[T] {
  def first: T = l.head
  def rest: input.Reader[T] = ListReader(l.tail)
  def pos: Position = NoPosition
  def atEnd: Boolean = l.isEmpty
}

trait ListParsers extends Parsers {
  type Elem = String
  
  def regex(r: Regex): Parser[Elem] = acceptIf(e => r.findFirstIn(e).isDefined)(e => s"Didn't match $e")
  def parse[T](p: Parser[T], l: List[String]): ParseResult[T] = p(ListReader(l))
  
}

object OptionParsers extends RegexParsers {
  
  sealed trait Name
  case class Long(n: String) extends Name
  case class Short(c: Char) extends Name
  
  def show(n: Name): String = n match {
    case Long(n) => '"' + "--" + n + '"'
    case Short(n) => '"' + "-" + n.toString + '"'
  }
  
  case class OptParams(names: List[Name] = List(), doc: Option[String] = None, metavar: String = "ARG")
  
  case class Opt[T](parser: Parser[T], default: Option[T] = None, params: OptParams = OptParams()) {
    def matches(s: String): Boolean = params.names.exists {
      case Long(n) => s == "--" + n
      case Short(c) => s == "-" + c
    }
  }
  
  implicit def optionFunctor: Functor[Opt] = new Functor[Opt] {
    def map[T,U](fa: Opt[T])(f: T => U): Opt[U] = Opt(fa.parser.map(f), fa.default.map(f), fa.params)
  }
  
  sealed trait Prsr[T] {
    def mod(m: ModP[T]) = m(this)
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
    case NoSuccess(_, _) => {println("parse failed"); None }
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

  def option[T](p: Parser[T], m: ModP[T]): Prsr[T] = m(OptP(Opt(p)))
  def default[T](t: T): Prsr[T] = t.point[Prsr]
  def strOpt(name: String): Prsr[String] = option(""".*""".r, long(name))
  def intOpt(name: String): Prsr[Int] = option("""\d+""".r ^^ {_.toInt}, long(name))
  
  //def flag[T](parser: Parser[T], default: T): Prsr[T] = active.point[Prsr] <+> default.point[Prsr]
  
  def optionSearch[T](o: Opt[T], args: List[String]): Option[(T, List[String])] = args match {
    case name :: value :: tail => 
      if (o.matches(name)) toOption(parse(o.parser, value)).strengthR(tail)
      else optionSearch(o, tail).map { case (t, as) => (t, name :: value :: as) }
    case _ => None
  }
  
  def parse[T](p: Prsr[T], args: List[String]): Option[(T, List[String])] = p match {
    case Done(o) => o.strengthR(args)
    case OptP(opt) => optionSearch(opt, args)
    case Chain(opt, rest) => for {
      (f, rem) <- parse(rest, args)
      (v, rem2) <- parse(opt, rem)
    } yield (f(v), rem2)
    case Alt(left, right) => parse(left, args) <+> parse(right, args)
  }
  
  def updateOpt[T](f: OptParams=>OptParams, p: Prsr[T]): Prsr[T] = p match {
    case OptP(o) => OptP(o.copy(params = f(o.params)))
    case Alt(left, right) => updateOpt(f, left) <+> updateOpt(f, right)
    case o => o
  }
  
  trait Mod[T] {
    val underlying: T=>T
    def apply(o: T) = underlying(o)
    def |+|(b: => Mod[T]): Mod[T] = Mod(underlying compose b.underlying)
  }
  
  object Mod {
    def apply[T](f: T => T) = new Mod[T] { val underlying = f }
    def optMod[T](f: OptParams=>OptParams): ModP[T] = Mod(updateOpt(f, _))
  }
  
  type ModP[T] = Mod[Prsr[T]]
  
  implicit def modMonoid[T]: Monoid[Mod[T]] = new Monoid[Mod[T]] {
    def zero: Mod[T] = Mod(identity)
    def append(a: Mod[T], b: => Mod[T]): Mod[T] = Mod(a.underlying compose b.underlying)
  }
  def metavar[T](meta: String): ModP[T] = Mod.optMod(_.copy(metavar=meta))
  def doc[T](value: String): ModP[T] = Mod.optMod(_.copy(doc=Some(value)))
  def long[T](name: String): ModP[T] = Mod.optMod(ps => ps.copy(names=Long(name)::ps.names))
  def short[T](name: Char): ModP[T] = Mod.optMod(ps => ps.copy(names=Short(name)::ps.names))
  
  case class User(name: String, id: Int)
  
  val testParser: Prsr[User] = (
      (strOpt("name") <+> strOpt("-n") <+> default("John").mod(metavar("NME")))
      |@| (intOpt("id"))
      ) { User.apply }
  
  def getDoc[T](p: Prsr[T]): Option[String] = p match {
    case Done(v) => None
    case OptP(o) => o.params.doc
    case Alt(l,r) => {
      val ld = getDoc(l)
      val rd = getDoc(r)
      if (ld == rd) ld else (ld |@| rd) { _ + "\n" + _}
    }
    // Never document a chain
    case Chain(opt, rest) => None
  }
  
  def show[T](p: Prsr[T]): Option[String] = p match {
    case Done(v) => None
    case OptP(o) =>  Some(s"${o.params.names.map(show)} ${o.params.metavar}")
    case Alt(l,r) => for {
      left <- show(l)
      right = show(r).fold("")(" | " + _)
    } yield left + right
    case Chain(opt, rest) => for {
      r <- show(opt)
      o = show(rest).fold("")("\n" + _)
    } yield r + o
  }
}
