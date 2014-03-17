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

trait Eval {
  type Error = String
  type Result[+T] = String \/ T
  type StateTResult[S, T] = StateT[Result, S, T]
  type ArgStateT[G[+_], T] = StateT[G, List[String], T]
  type Eval[T] = StateTResult[List[String], T]
  implicit def plusEval: Plus[Eval] = new Plus[Eval] {
    // do both with the same state and take the first one if it succeeds, otherwise the second
    def plus[A](a: Eval[A], b: => Eval[A]): Eval[A] = StateT(s => a.run(s) <+> b.run(s))
  }
  def lift[T](o: Result[T]): Eval[T] = o.liftM[ArgStateT]
  val ms = MonadState[StateTResult, List[String]]
}

case class ListReader[T](l: List[T]) extends input.Reader[T] {
  def first: T = l.head
  def rest: input.Reader[T] = ListReader(l.tail)
  def pos: Position = NoPosition
  def atEnd: Boolean = l.isEmpty
}

trait ListParsers extends Parsers with Eval {
  type Elem = String
  val m = parserMonad(this)
  def mkList(r: input.Reader[Elem]): List[Elem] = if (r.atEnd) Nil else r.first :: mkList(r.rest)
  
  def regex(r: Regex): Parser[Elem] = acceptIf(e => r.findFirstIn(e).isDefined)(e => s"Didn't match $e")
  def parseEval[T](p: Parser[T], l: List[String]): Eval[T] = p(ListReader(l)) match {
    case Success(v, rest) => constantStateT(v)(mkList(rest))
    case NoSuccess(msg, rest) => lift(msg.left)
  }
  
  def parseEval[T](p: Parser[T]): Eval[T] = for {
    l <- ms.get
    r <- parseEval(p, l)
  } yield r
    
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

object OptionParsers extends ListParsers with Eval {
  
  sealed trait Name
  case class Long(n: String) extends Name
  case class Short(c: Char) extends Name
  case class Plain(n: String) extends Name
  
  def show(n: Name): String = n match {
    case Long(n) => "--" + n
    case Short(n) => "-" + n.toString
    case Plain(n) => n
  }
  
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

  def option[T](p: Parser[T]): OptP[T] = OptP(OptAndArgs(p))
  def opt[T](name: String)(implicit p: Parser[T]): Prsr[T] = option(p).mod(long(name))
  def flag[T](yes: T, no: T): OptP[T] = OptP(OptAndArgs(success(yes), Some(no)))
  def flag[T](name: Char, yes: T, no: T): Prsr[T] = OptP(OptAndArgs(success(yes), Some(no))).mod(short(name))
  def arg[T](implicit p: Parser[T]): OptP[T] = argument(p)
  def argument[T](p: Parser[T]): OptP[T] = OptP(Arg(p))
  implicit val strOpt: Parser[String] = regex(""".*""".r)
  implicit val intOpt: Parser[Int] = regex("""\d+""".r) ^^ {_.toInt}
  
  def optSearch[T](o: OptAndArgs[T]): Eval[T] = for {
    _ <- ms.modify(_.dropWhile(arg => !(arg.startsWith("-"))))
    args <- ms.get
    value <- args match {
      case name :: tail =>
        if (matches(name, o.params.names)) parseEval(o.parser, tail)
        else for {
          _ <- ms.put(tail)
          v <- optSearch(o)
          _ <- ms.modify(name ::  _)
        } yield v
      case _ => lift(o.default.toRightDisjunction("No value found"))
    }
  } yield value
  
  def doOpt[T](o: Opt[T]): Eval[T] = o match {
    case o@OptAndArgs(p, d, pa) => optSearch(o)
    case a@Arg(p) => parseEval(p)
  }
  
  def eval[T](p: => Prsr[T]): Eval[T] = p match {
    case Done(o) => lift(o)
    case OptP(opt) => doOpt(opt)
    case Chain(opt, rest) => eval(opt) <*> eval(rest)
    case Alt(left, right) => eval(left) <+> eval(right)
    case Bind(arg, bound) => for {
      arg <- eval(arg)
      mapped = bound(arg)
      evaled <- eval(mapped)
    } yield evaled
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
    def paramMod[T](f: OptParams=>OptParams): ModP[T] = Mod(updateOpt({ 
      case OptAndArgs(p,d,pa) => OptAndArgs(p,d,f(pa))
      case o => o
    }, _))
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
  def default[T](value: T): ModP[T] = Mod.optMod({ 
    case OptAndArgs(p,d,pa) => OptAndArgs(p,Some(value),pa)
    case o => o
  })
  
  def sep(pre: String, sep: String, post: String) = {
    val sepp = if (pre != "" && post != "") sep else ""
    pre + sepp + post
  }
  
  def format[T](o: Opt[T]): Option[String] = o match {
    case OptAndArgs(p,d,pa) => pa.names.headOption.map(show).map(sep(_, " ", pa.metavar))
    case Arg(p) => Some("ARG")
  }
  
  def isOptional[T](o: Opt[T]): Boolean = o match {
    case OptAndArgs(p,d,pa) => d.isDefined
    case Arg(p) => false
  }
  
  def help[T](p: Prsr[T]): String = p match {
    case Done(v) => ""
    case OptP(o) => o match {
      case OptAndArgs(p, d, pa) =>(for {
        doc <- pa.doc
        args <- format(o)
      } yield s"$args \n $doc") getOrElse ""
      case Arg(p) => ""
    } 
    case Alt(l,r) => sep(help(l), " | ", help(r))
    case Chain(opt, rest) => sep(help(rest), "\n", help(opt))
  }
  
  def usage[T](p: Prsr[T]): String = p match {
    case Done(v) => ""
    case OptP(o) => (for {
      f <- format(o)
    } yield if (isOptional(o)) s"[$f]" else f) getOrElse ""
    case Alt(l,r) => s"alt(${usage(l)} ${usage(r)})"
    case Chain(opt, rest) => sep(usage(rest), " ", usage(opt))
  }
  
  def allHelp[T](p: Prsr[T]): String = "Usage:" + usage(p) + "\n" + "Options: " + help(p)
  
  def helper[T](p: Prsr[T]): Prsr[T] = p <*> option(failure(allHelp(p))).mod(long("help") |+| short('h'))
  
  sealed trait IsAdmin
  case object Yes extends IsAdmin
  case object No extends IsAdmin
  case object Maybe extends IsAdmin
  
  case class User(name: String, id: Int, admin: IsAdmin, other: List[Int])
  
  val testParser: Prsr[User] = (
      opt[String]("name").mod(short('n') |+| metavar("NME") |+| doc("The user's name"))
      |@| opt[Int]("id").mod(default(0) |+| doc("The user's id"))
      |@| flag[IsAdmin]('a', Yes, No).mod(doc("Is the user an admin?"))
      |@| pa.many(arg[Int])
      ) { User.apply }
}
