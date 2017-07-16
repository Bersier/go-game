package board

import main.Config

sealed trait Move extends Any

case object Pass extends Move

//final class Intersection private(val i: Byte, val j: Byte) extends Move {
//  assert(i >= 0)
//  assert(j >= 0)
//
//  /**
//    * @param size of the board
//    * @return all the neighbors of this intersection
//    */
//  def neighbors(implicit size: Size): Traversable[Intersection] = new Traversable[Intersection] {
//    override def foreach[U](f: (Intersection) => U): Unit = {
//      if (i > 0) f(Intersection(i - 1, j))
//      if (j > 0) f(Intersection(i, j - 1))
//      if (i + 1 < Intersection.this.size) f(Intersection(i + 1, j))
//      if (j + 1 < Intersection.this.size) f(Intersection(i, j + 1))
//    }
//  }
//
//  override def equals(any: Any): Boolean = any match {
//    case that: Intersection => this.i == that.i && this.j == that.j
//    case _ => false
//  }
//
//  override def hashCode():Int = {
//    i * ((1<<16) + 1) + j
//  }
//
//  override def toString = s"[$i,$j]"
//}
//
//object Intersection {
//
//  def apply(i: Int, j: Int): Intersection = new Intersection(i.toByte, j.toByte)
//
//  def unapply(x: Intersection): Option[(Int, Int)] = Some(x.i, x.j)
//}

final class Intersection private(val x: Int) extends AnyVal with Move {

  def i: Int = x >>> 2*Intersection.shift
  def j: Int = x >>>   Intersection.shift

  /**
    * @return all the neighbors of this intersection
    */
  def neighbors: Traversable[Intersection] = new Traversable[Intersection] {
    override def foreach[U](f: (Intersection) => U): Unit = {
      if (i > 0) {
        f(new Intersection(x & Intersection.sjMask | i - 1 << 2*Intersection.shift))
      }
      if (j > 0) {
        f(new Intersection(x & Intersection.siMask | j - 1 <<   Intersection.shift))
      }
      if (i + 1 < Intersection.this.size) {
        f(new Intersection(x & Intersection.sjMask | i + 1 << 2*Intersection.shift))
      }
      if (j + 1 < Intersection.this.size) {
        f(new Intersection(x & Intersection.siMask | j + 1 <<   Intersection.shift))
      }
    }
  }

  def toString(implicit size: Size): String = s"[$i,$j]"

  private[this] implicit def size: Size = Size(x & Intersection.maxSize)
}

object Intersection {
  private val shift = 10
  private val maxSize = (1 << shift) - 1
  private val sjMask = maxSize | maxSize <<   shift
  private val siMask = maxSize | maxSize << 2*shift
  assert(Config.maxSize <= maxSize)

  def apply(i: Int, j: Int)(implicit size: Size): Intersection = {
    assert(i >= 0)
    assert(j >= 0)
    assert(i < size)
    assert(j < size)
    new Intersection((size: Int) + ((j + (i << shift)) << shift))
  }

  def unapply(x: Intersection): Option[(Int, Int)] = Some((x.i, x.j))
}