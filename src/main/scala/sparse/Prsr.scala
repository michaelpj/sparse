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
import scalaz.syntax.all._
import scalaz.syntax.state._
import Id._

trait Eval {
  type StateTOption[S, T] = StateT[Option, S, T]
  type ArgStateT[G[+_], T] = StateT[G, List[String], T]
  type Eval[T] = StateTOption[List[String], T]
  implicit def plusEval: Plus[Eval] = new Plus[Eval] {
    // do both with the same state and take the first one if it succeeds, otherwise the second
    def plus[A](a: Eval[A], b: => Eval[A]): Eval[A] = StateT(s => a.run(s) <+> b.run(s))
  }
  def lift[T](o: Option[T]): Eval[T] = o.liftM[ArgStateT]
  val ms = MonadState[StateTOption, List[String]]
}

case class ListReader[T](l: List[T]) extends input.Reader[T] {
  def first: T = l.head
  def rest: input.Reader[T] = ListReader(l.tail)
  def pos: Position = NoPosition
  def atEnd: Boolean = l.isEmpty
}

trait ListParsers extends Parsers with Eval {
  type Elem = String
  def mkList(r: input.Reader[Elem]): List[Elem] = if (r.atEnd) Nil else r.first :: mkList(r.rest)
  
  def regex(r: Regex): Parser[Elem] = acceptIf(e => r.findFirstIn(e).isDefined)(e => s"Didn't match $e")
  def parseEval[T](p: Parser[T], l: List[String]): Eval[T] = p(ListReader(l)) match {
    case Success(v, rest) => constantStateT(v)(mkList(rest))
    case NoSuccess(_, rest) => lift(None)
  }
}

object OptionParsers extends ListParsers with Eval {
  
  sealed trait Name
  case class Long(n: String) extends Name
  case class Short(c: Char) extends Name
  
  def show(n: Name): String = n match {
    case Long(n) => "--" + n
    case Short(n) => "-" + n.toString
  }
  
  def matches(s: String, names: List[Name]): Boolean = names.exists {
      case Long(n) => s == "--" + n
      case Short(c) => s == "-" + c
    }
  
  /*sealed trait OptReader[T]
  case class OptAndArgs[T](parser: Parser[T], names: List[Name] = List()) extends OptReader[T]
  
  implicit def readerFunctor= new Functor[OptReader] {
    def map[A,B](fa: OptReader[A])(f: A=>B): OptReader[B] = fa match {
      case OptAndArgs(p, l) => OptAndArgs(p.map(f), l)
    }
  }*/
  
  case class OptParams(doc: Option[String] = None, metavar: String = "ARG")
  
  sealed trait Opt[T]
  case class OptAndArgs[T](parser: Parser[T], default: Option[T] = None, names: List[Name] = List(), params: OptParams = OptParams()) extends Opt[T]
  
  implicit def optionFunctor: Functor[Opt] = new Functor[Opt] {
    def map[T,U](fa: Opt[T])(f: T => U): Opt[U] = fa match {
      case OptAndArgs(p, d, n, pa) => OptAndArgs(p.map(f), d.map(f), n, pa)
    }
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

  def option[T](p: Parser[T]): OptP[T] = OptP(OptAndArgs(p))
  def strOpt(name: String): Prsr[String] = long(name)(option(regex(""".*""".r)))
  def intOpt(name: String): Prsr[Int] = long(name)(option(regex("""\d+""".r) ^^ {_.toInt}))
  
  //def flag[T](parser: Parser[T], default: T): Prsr[T] = active.point[Prsr] <+> default.point[Prsr]
  
  def optSearch[T](o: OptAndArgs[T]): Eval[T] = for {
    args <- ms.get
    value <- args match {
      case name :: tail => { println(name); println(o.names);
        if (matches(name, o.names)) { println("matched" + name ) ; parseEval(o.parser, tail) }
        else for {
          _ <- ms.put(tail)
          v <- optSearch(o)
          _ <- ms.modify(name ::  _)
        } yield v }
      case _ => lift(o.default)
    }
  } yield value
  
  def doOpt[T](o: Opt[T]): Eval[T] = o match {
    case o@OptAndArgs(p, d, n, pa) => for {
      _ <- ms.modify(_.dropWhile(arg => !(arg.startsWith("-"))))
      v <- optSearch(o)
    } yield v
  }
  
  def eval[T](p: Prsr[T]): Eval[T] = p match {
    case Done(o) => lift(o)
    case OptP(opt) => doOpt(opt)
    case Chain(opt, rest) => for {
      f <- eval(rest)
      v <- eval(opt)
    } yield f(v)
    case Alt(left, right) => eval(left) <+> eval(right)
  }
  
  def updateOpt[T](f: Opt[T]=>Opt[T], p: Prsr[T]): Prsr[T] = p match {
    case OptP(o) => OptP(f(o))
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
    def paramMod[T](f: OptParams=>OptParams): ModP[T] = Mod(updateOpt(o => o.copy(params = f(o.params)), _))
    def optMod[T](f: Opt[T]=>Opt[T]): ModP[T] = Mod(updateOpt(f, _))
  }
  
  type ModP[T] = Mod[Prsr[T]]
  
  implicit def modMonoid[T]: Monoid[Mod[T]] = new Monoid[Mod[T]] {
    def zero: Mod[T] = Mod(identity)
    def append(a: Mod[T], b: => Mod[T]): Mod[T] = Mod(a.underlying compose b.underlying)
  }
  def metavar[T](meta: String): ModP[T] = Mod.paramMod(_.copy(metavar=meta))
  def doc[T](value: String): ModP[T] = Mod.paramMod(_.copy(doc=Some(value)))
  def long[T](name: String): ModP[T] = Mod.paramMod(ps => ps.copy(names=Long(name)::ps.names))
  def short[T](name: Char): ModP[T] = Mod.paramMod(ps => ps.copy(names=Short(name)::ps.names))
  def default[T](value: T): ModP[T] = Mod.optMod({ case OptAndArgs(p,d,n,pa) => OptAndArgs(p,Some(value),n,pa) })
  
  case class User(name: String, id: Int)
  
  val testParser: Prsr[User] = (
      strOpt("name").mod(short('n') |+| metavar("NME") |+| doc("The user's name"))
      |@| intOpt("id").mod(default(0) |+| doc("The user's id"))
      ) { User.apply }
  
  def format[T](o: Opt[T]): String = o match {
    case OptAndArgs(p,d,n,pa) => n.map(show).mkString("|") + " " + pa.metavar
  }
  
  def isOptional[T](o: Opt[T]): Boolean = o match {
    case OptAndArgs(p,d,n,pa) => d.isDefined
  }
  
  def help[T](p: Prsr[T]): String = p match {
    case Done(v) => ""
    case OptP(o) => o match {
      case OptAndArgs(p, d, n, pa) =>(for {
        doc <- pa.doc
        args = format(o)
      } yield s"$args \n $doc") getOrElse ""
    } 
    case Alt(l,r) => {
      val left = help(l)
      val right = help(r)
      left + (if (right != "") " | " else "") + right
    }
    case Chain(opt, rest) => {
      val r = help(rest)
      val o = help(opt)
      r + (if (r != "") "\n" else "") + o
    }
  }
  
  def usage[T](p: Prsr[T]): String = p match {
    case Done(v) => ""
    case OptP(o) => {
      val f = format(o)
      if (isOptional(o)) s"[$f]" else f
    }
    case Alt(l,r) => s"alt(${usage(l)} ${usage(r)})"
    case Chain(opt, rest) => usage(rest) + usage(opt)
  }
}
