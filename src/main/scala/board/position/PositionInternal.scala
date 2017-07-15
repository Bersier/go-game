package board.position

import board.{Black, Intersection, Move, None, Pass, ProperColor, Size, White}
import commons.Utils
import zobristcode.ZCode128

private trait PositionInternal extends Position {

  final override
  def nextPositions(player: ProperColor)(implicit forbidden: Set[Position]): Iterator[Position] = {
    for (x <- this.intersections; n <- withMove(x, player)) yield n
  }

  final override
  def withMove(move: Move, player: ProperColor)
              (implicit prev: Set[Position]): Position = move match {
    case x: Intersection => {
      assert(apply(x) == None, s"Illegal move: $x is already occupied")
      assert(prev(this), "'previous' should contain the current position")
      val result = nextPositionBuilder.playAt(x, player).build
      assert(!prev(result), s"Illegal move: results in a previous position: $result")
      result
    }
    case Pass => this
  }

  protected[position] implicit def size: Size

  /**
    * @return a builder to build the next position, that starts from the current position
    */
  protected[this] def nextPositionBuilder: Builder

  private[this] def intersections: Iterator[Intersection] = {
    for (k <- Utils.cheapShuffledRange(size * size).iterator) yield {
      Intersection(k / size, k % size)
    }
  }

  private[this] def withMove(x: Intersection, color: ProperColor)
                            (implicit forbidden: Set[Position]): Option[Position] = this(x) match {
    case None => {
      val result = nextPositionBuilder.playAt(x, color).build
      if (result == this || forbidden(result)) Option.empty
      else Some(result)
    }
    case _ => Option.empty
  }

  override def toZobristCode: ZCode128 = ZobristCoder.get.computeCode((i, j) => this(i, j))

  override def equals(other: Any): Boolean = other match {
    case that: Position => this.toZobristCode == that.toZobristCode
    case _ => false
  }

  override def hashCode: Int = toZobristCode._2.toInt

  override def toString: String = {
    val builder = StringBuilder
    for (i <- 0 until size) {
      builder + "["
      for (j <- 0 until size) {
        builder + (this(i, j) match {
          case Black => "●"
          case White => "○"
          case None  => " "
        })
      }
      builder + "]"
    }
    builder.toString
  }
}
