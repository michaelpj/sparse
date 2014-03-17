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

trait Eval extends Prsrs {
  
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

object OptionParsers extends Eval {
  def parseEval[T](p: Parser[T], l: List[String]): Eval[T] = p(ListReader(l)) match {
    case Success(v, rest) => constantStateT(v)(mkList(rest))
    case NoSuccess(msg, rest) => lift(msg.left)
  }
  
  def parseEval[T](p: Parser[T]): Eval[T] = for {
    l <- ms.get
    r <- parseEval(p, l)
  } yield r
  

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
  
  def paramMod[T](f: OptParams=>OptParams): ModP[T] = Mod(updateOpt({ 
    case OptAndArgs(p,d,pa) => OptAndArgs(p,d,f(pa))
    case o => o
  }, _))
  def optMod[T](f: Opt[T]=>Opt[T]): ModP[T] = Mod(updateOpt(f, _))
  
  def metavar[T](meta: String): ModP[T] = paramMod(_.copy(metavar=meta))
  def doc[T](value: String): ModP[T] = paramMod(_.copy(doc=Some(value)))
  def long[T](name: String): ModP[T] = paramMod(ps => ps.copy(names=Long(name)::ps.names))
  def short[T](name: Char): ModP[T] = paramMod(ps => ps.copy(names=Short(name)::ps.names))
  def default[T](value: T): ModP[T] = optMod({ 
    case OptAndArgs(p,d,pa) => OptAndArgs(p,Some(value),pa)
    case o => o
  })
  
  import Printer._
  def helper[T](p: Prsr[T]): Prsr[T] = p <*> option(failure(pretty(allHelp(p)))).mod(long("help") |+| short('h'))
  
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
