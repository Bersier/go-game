package board

sealed trait Move

case object Pass extends Move

final class Intersection private(val i: Byte, val j: Byte) extends Move {
  require(i >= 0)
  require(j >= 0)

  /**
    * @param size of the board
    * @return all the neighbors of this intersection
    */
  def neighbors(implicit size: Size): Traversable[Intersection] = new Traversable[Intersection] {
    override def foreach[U](f: (Intersection) => U): Unit = {
      if (i > 0) f(Intersection(i - 1, j))
      if (j > 0) f(Intersection(i, j - 1))
      if (i + 1 < size) f(Intersection(i + 1, j))
      if (j + 1 < size) f(Intersection(i, j + 1))
    }
  }

  override def equals(any: Any): Boolean = any match {
    case that: Intersection => this.i == that.i && this.j == that.j
    case _ => false
  }

  override def hashCode():Int = {
    i * ((1<<16) + 1) + j
  }

  override def toString = s"[$i,$j]"
}

object Intersection {

  def apply(i: Byte, j: Byte): Intersection = new Intersection(i, j)

  def apply(i: Int, j: Int): Intersection = new Intersection(i.toByte, j.toByte)

  def unapply(arg: Intersection): Option[(Int, Int)] = Some(arg.i, arg.j)
}