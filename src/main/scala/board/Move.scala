package board

sealed trait Move

case object Pass extends Move

final case class Intersection(i: Byte, j: Byte) extends Move {
  require(i >= 0)
  require(j >= 0)

  /**
    * @param size of the board
    * @return all the neighbors of this intersection
    */
  def neighbors(implicit size: Size): Traversable[Intersection] = new Traversable[Intersection] {
    override def foreach[U](f: (Intersection) => U): Unit = {
      if (i > 0) f(Intersection((i - 1).toByte, j))
      if (j > 0) f(Intersection(i, (j - 1).toByte))
      if (i + 1 < size) f(Intersection((i + 1).toByte, j))
      if (j + 1 < size) f(Intersection(i, (j + 1).toByte))
    }
  }

  override def equals(any: Any): Boolean = any match {
    case that: Intersection => this.i == that.i && this.j == that.j
    case _ => false
  }

  override def hashCode():Int = {
    i * ((1<<16) + 1) + j
  }
}