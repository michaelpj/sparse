package sparse

import scalaz.Monoid

trait Mod[T] {
  val underlying: T=>T
  def apply(o: T) = underlying(o)
  def |+|(b: => Mod[T]): Mod[T] = Mod(underlying compose b.underlying)
}
  
object Mod {
  def apply[T](f: T => T) = new Mod[T] { val underlying = f }
  implicit def modMonoid[T]: Monoid[Mod[T]] = new Monoid[Mod[T]] {
    def zero: Mod[T] = Mod(identity)
    def append(a: Mod[T], b: => Mod[T]): Mod[T] = Mod(a.underlying compose b.underlying)
  }
}
