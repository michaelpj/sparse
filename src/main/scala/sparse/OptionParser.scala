package sparse

import scala.util.parsing.combinator
import scala.util.parsing.combinator.RegexParsers
import scalaz.std.util.parsing.combinator.parser._
import scalaz._
import scalaz.syntax.applicative._

import scala.language.existentials

object OptionParsers extends RegexParsers {
  case class OptProps(name: String, help: String = "", metavar: String = "")
  case class Opt[T](props: OptProps, parser: Parser[T]) {
    def matches(s: String): Boolean = s == "--" + props.name
  }
  
  implicit def optionFunctor[T]: Functor[Opt] = new Functor[Opt] {
    def map[U](fa: Opt[T])(f: T => U): Opt[U] = Opt(fa.props, fa.parser.map(f))
  }
  
  trait OptionParser[+T]
  case class NilP[T](t: Option[T]) extends OptionParser[T]
  case class ConsP[T](opt: OptionParser[_ => T], rest: OptionParser[_]) extends OptionParser[T]
  
  implicit def parserFunctor[T]: Functor[OptionParser] = new Functor[OptionParser] {
    def map[U](fa: OptionParser[T])(f: T => U): OptionParser[U] = fa match {
      case NilP(o) => NilP(o.map(f))
      case ConsP(opt, rest) => ConsP(opt.map(f.compose(_)), rest)
    }
  }
  
  def uncurry[A,B,C](f: A => (B=>C)): ((A,B) => C) = (a, b) => f(a)(b)
  def const[T, U](u: U): T=>U = t => u
  
  implicit def parserApplicative[T]: Applicative[OptionParser] = new Applicative[OptionParser] {
    def point(t: T): OptionParser[T] = NilP(Some(t))
     def ap[A,B](fa: => OptionParser[A])(f: => OptionParser[A => B]): OptionParser[B] = ConsP(f, fa)
  }
  
  def option[T](name: String, parser: Parser[T]): OptionParser[T] = ConsP(Opt(OptProps(name), parser).map(const[Nothing, T](_)).point[OptionParser], NilP(Some(())))
  
  case class User(name: String, id: Int)
  
  val testParser: OptionParser[User] = (option("name", ".*") |@| option("id", "\\d+" ^^ { _.toInt })) { User.apply }
}
