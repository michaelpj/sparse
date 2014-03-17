package sparse

import scala.util.parsing.combinator
import scala.util.parsing.combinator.RegexParsers
import scalaz.std.util.parsing.combinator.parser._
import scalaz._

import scalaz.std.option._
import scala.language.existentials
import scala.language.higherKinds
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import StateT._
import \/._
import scalaz.syntax.all._
import scalaz.syntax.std.option._
import scalaz.syntax.state._
import Id._
import Mod._

case class ListReader[T](l: List[T]) extends input.Reader[T] {
  def first: T = l.head
  def rest: input.Reader[T] = ListReader(l.tail)
  def pos: Position = NoPosition
  def atEnd: Boolean = l.isEmpty
}

trait ListParsers extends Parsers {
  type Elem = String
  val m = parserMonad(this)
  def mkList(r: input.Reader[Elem]): List[Elem] = if (r.atEnd) Nil else r.first :: mkList(r.rest)
  
  def regex(r: Regex): Parser[Elem] = acceptIf(e => r.findFirstIn(e).isDefined)(e => s"Didn't match $e")

  implicit def parserPlus = new ApplicativePlus[Parser] {
    def point[A](a: => A): Parser[A] = success(a)
    def ap[A,B](a: => Parser[A])(b: => Parser[A=>B]): Parser[B] = for {
      aa <- a
      bb <- b
    } yield bb(aa)
    def plus[A](a: Parser[A], b: => Parser[A]): Parser[A] = a | b
    def empty[A]: Parser[A] = failure("None")
  }
  
}

trait Prsrs extends ListParsers {
    
  type Error = String
  type Result[+T] = String \/ T
  
  sealed trait Name
  case class Long(n: String) extends Name
  case class Short(c: Char) extends Name
  case class Plain(n: String) extends Name
  
  def matches(s: String, names: List[Name]): Boolean = names.exists {
      case Long(n) => s == "--" + n
      case Short(c) => s == "-" + c
      case Plain(n) => s == n 
    }
  
  case class OptParams(names: List[Name] = List(), doc: Option[String] = None, metavar: String = "")
  
  sealed trait Opt[T]
  case class OptAndArgs[T](parser: Parser[T], default: Option[T] = None, params: OptParams = OptParams()) extends Opt[T]
  case class Arg[T](parser: Parser[T]) extends Opt[T]
  
  implicit def optionFunctor: Functor[Opt] = new Functor[Opt] {
    def map[T,U](fa: Opt[T])(f: T => U): Opt[U] = fa match {
      case OptAndArgs(p, d, pa) => OptAndArgs(p.map(f), d.map(f), pa)
      case Arg(p) => Arg(p.map(f))
    }
  }
  
  type ModP[T] = Mod[Prsr[T]]
  
  sealed trait Prsr[T] {
    def mod(m: ModP[T]) = m(this)
  }
  case class Done[T](t: Result[T]) extends Prsr[T]
  trait Chain[T] extends Prsr[T] {
    type U
    val opt: Prsr[U]
    val rest: Prsr[U=>T]
  }
  object Chain {
    def apply[T,U2](opt2: Prsr[U2], rest2: Prsr[U2=>T]) = new Chain[T] {
      type U = U2
      val opt: Prsr[U] = opt2
      val rest: Prsr[U => T] = rest2
    }
    def unapply[T](c: Chain[T]): Option[(Prsr[U], Prsr[U=>T])] forSome { type U } = Some((c.opt, c.rest))
  }
  case class OptP[T](opt: Opt[T]) extends Prsr[T]
  case class Alt[T](left: Prsr[T], right: Prsr[T]) extends Prsr[T]
  // this is an opaque case that won't be statically evaluated, so it covers
  // subcommands and manys
  trait Bind[T] extends Prsr[T] {
    type U
    val arg: Prsr[U]
    val bound: U => Prsr[T]
  }
  object Bind {
    def apply[T,U2](arg2: Prsr[U2], bound2: U2 => Prsr[T]) = new Bind[T] {
      type U = U2
      val arg: Prsr[U] = arg2
      val bound: U => Prsr[T] = bound2
    }
    def unapply[T2](c: Bind[T2]): Option[(Prsr[U], U=>Prsr[T2])] forSome { type U } = Some((c.arg, c.bound))
  }
  
  implicit def pa: ApplicativePlus[Prsr] = new ApplicativePlus[Prsr] {
    // more efficient map
    override def map[T, U](fa: Prsr[T])(f: T => U): Prsr[U] = fa match {
      case Done(o) => Done(o.map(f))
      case OptP(opt) => OptP(opt.map(f))
      case Chain(opt, rest) => Chain(opt, rest.map(f.compose(_)))
      case Alt(left, right) => Alt(left.map(f), right.map(f))
      case Bind(arg, bound) => Bind(arg, bound andThen (_.map(f)))
    }
    def point[T](t: => T): Prsr[T] = Done(t.right)
    def ap[A,B](fa: => Prsr[A])(f: => Prsr[A => B]): Prsr[B] = Chain(fa, f)
    def plus[A](a: Prsr[A], b: => Prsr[A]): Prsr[A] = Alt(a, b)
    def empty[A]: Prsr[A] = Done("Nothing".left)
    
    override def many[A](a: Prsr[A]): Prsr[List[A]] = {
      Bind[List[A], Option[A]](optional(a), {
        case Some(o) => Bind(List[A](o).point[Prsr], (as: List[A]) => (point(as) |@| many(a)) { _ ++ _ })
        case None => List[A]().point[Prsr]
      })
    }
  }
  
  def optional[A](fa: Prsr[A]): Prsr[Option[A]] = fa.map[Option[A]](Some(_)) <+> None.asInstanceOf[Option[A]].point[Prsr]

}