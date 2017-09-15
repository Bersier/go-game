package board

import main.Config

sealed trait Move extends Any

case object Pass extends Move

final class Intersection private(val x: Int) extends AnyVal with Move {

  def i: Int = x >>> Intersection.shift

  def j: Int = x & Intersection.jMask

  /**
    * @return all the neighbors of this intersection
    */
  def neighbors(implicit s: Size): Traversable[Intersection] = new Traversable[Intersection] {
    override def foreach[U](f: (Intersection) => U): Unit = {
      if (i > 0) {
        f(new Intersection(x & Intersection.jMask | i - 1 << Intersection.shift))
      }
      if (j > 0) {
        f(new Intersection(x & Intersection.iMask | j - 1))
      }
      if (i + 1 < s) {
        f(new Intersection(x & Intersection.jMask | i + 1 << Intersection.shift))
      }
      if (j + 1 < s) {
        f(new Intersection(x & Intersection.iMask | j + 1))
      }
    }
  }

  override def toString: String = s"[$i,$j]"
}

object Intersection {
  private val shift = 16
  private val maxSize = (1 << shift) - 1
  private val jMask = maxSize
  private val iMask = maxSize << shift
  assert(Config.maxSize <= maxSize)

  def apply(i: Int, j: Int): Intersection = {
    assert(i >= 0)
    assert(j >= 0)
    new Intersection(j + (i << shift))
  }

  def fromIndex(index: Int)(implicit size: Size): Intersection = {
    assert(index >= 0)
    assert(index <= size*size)
    new Intersection(index % size + (index / size << shift))
  }

  def apply(ij: (Int, Int)): Intersection = {
    assert(ij._1 >= 0)
    assert(ij._2 >= 0)
    new Intersection(ij._2 + (ij._1 << shift))
  }

  def unapply(x: Intersection): Option[(Int, Int)] = Some((x.i, x.j))
}