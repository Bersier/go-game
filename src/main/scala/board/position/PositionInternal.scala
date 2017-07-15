package board.position

import board.{Black, Intersection, Move, None, Pass, ProperColor, White}
import commons.Utils
import main.Config.size

private trait PositionInternal extends Position {

  final override
  def nextPositions(player: ProperColor)(implicit forbidden: Set[Position]): Iterator[Position] = {
    for (x <- this.intersections; n <- withMove(x, player)) yield n
  }

  final override
  def withMove(move: Move, player: ProperColor)
              (implicit prev: Set[Position]): Position = move match {
    case x: Intersection => {
      require(apply(x) == None, s"Illegal move: $x is already occupied")
      require(prev(this), "'previous' should contain the current position")
      val builder = nextPositionBuilder
      builder.update(x, player)
      builder.cleanup(x, player)
      val result = builder.build
      require(!prev(result), s"Illegal move: results in a previous position: $result")
      result
    }
    case Pass => this
  }

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
      val builder = nextPositionBuilder
      builder.update(x, color)
      builder.cleanup(x, color)
      val result = builder.build
      if (result == this || forbidden(result)) Option.empty
      else Some(result)
    }
    case _ => Option.empty
  }

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

  override def equals(other: Any): Boolean = other match {
    case that: Position => 0 until size forall {
      i => 0 until size forall {
        j => this (i, j) == that(i, j)
      }
    }
    case _ => false
  }

  override def hashCode: Int = toZobristCode._2.toInt
}
