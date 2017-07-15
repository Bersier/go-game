package commons

import scala.collection.mutable

sealed trait Memoizer[A, B] extends Function[A, B]

object Memoizer {
  def apply[A, B](f: A => B): Memoizer[A, B] = new GenMemoizer(f)
  def apply[A, B: reflect.ClassTag](f: Int => B)(memEnd: Int): Memoizer[Int, B] = {
    new IntMemoizer(f, memEnd)
  }
}

private final class GenMemoizer[A, B](f: A => B) extends Memoizer[A, B] {
  private[this] val mem = mutable.Map.empty[A, B]

  override def apply(a: A): B = mem.getOrElseUpdate(a, f(a))
}

private final
class IntMemoizer[B: reflect.ClassTag](f: Int => B, memEnd: Int) extends Memoizer[Int, B] {
  private[this] val mem = Array.ofDim[B](memEnd)

  override def apply(i: Int): B = {
    if (i < memEnd) {
      if (mem(i) == null) {
        mem(i) = f(i)
      }
      mem(i)
    }
    else f(i)
  }
}