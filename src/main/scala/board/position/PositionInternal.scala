package board.position

import board.{Black, Color, Intersection, Move, None, Pass, PlayerColor, Size, White}
import zobristcode.ZCode128

import scala.collection.mutable

private trait PositionInternal extends Position {

  final override
  def nextPositions(player: PlayerColor)(implicit forbidden: Set[Position]): Iterator[Position] = {
    for (x <- Position.intersections; n <- withMove(x, player)) yield n
  }

  final override
  def withMove(move: Move, player: PlayerColor)
              (implicit prev: Set[Position]): Position = move match {
    case x: Intersection => {
      require(apply(x) == None, s"Illegal move: $x is already occupied")
      require(prev(this), "'previous' should contain the current position")
      val result = nextPositionBuilder.playAt(x, player).build
      require(!prev(result), s"Illegal move: results in a previous position: $result")
      result
    }
    case Pass => this
  }

  /**
    * @return the size of the board
    */
  protected[position] implicit def size: Size

  /**
    * @return a builder to build the next position, that starts from the current position
    */
  protected[this] def nextPositionBuilder: Builder

  private[this] def withMove(x: Intersection, color: PlayerColor)
                            (implicit forbidden: Set[Position]): Option[Position] = this(x) match {
    case None => Some(nextPositionBuilder.playAt(x, color).build).filterNot(forbidden)
    case _ => Option.empty
  }

  override def toZobristCode: ZCode128 = ZobristCoder.get.computeCode((i, j) => this(i, j))

  override def count: Color => Int = {
    var noneCount  = 0
    var blackCount = 0
    var whiteCount = 0
    for (x <- Position.orderedIntersections) {
      this (x) match {
        case None  => noneCount  += 1
        case Black => blackCount += 1
        case White => whiteCount += 1
      }
    }
    {
      case None  => noneCount
      case Black => blackCount
      case White => whiteCount
    }
  }

  override def areas: PlayerColor => Int = {
    var blackCount = 0
    var whiteCount = 0
    val visited = mutable.Set.empty[Intersection]
    for (x <- Position.orderedIntersections) {
      this (x) match {
        case Black => blackCount += 1
        case White => whiteCount += 1
        case None  => if (!visited(x)) {
          val pocket = mutable.Set.empty[Intersection]
          val pocketBorderColors = mutable.Set.empty[PlayerColor]
          def setPocketBorderColors(x: Intersection): Unit = {
            if (!pocket.contains(x)) this (x) match {
              case None => {
                pocket += x
                x.neighbors.foreach(setPocketBorderColors)
              }
              case color: PlayerColor => {
                pocketBorderColors += color
              }
            }
          }
          setPocketBorderColors(x)
          visited ++= pocket
          if (pocketBorderColors.size == 1) {
            if (pocketBorderColors.contains(Black)) {
              blackCount += pocket.size
            }
            else {
              whiteCount += pocket.size
            }
          }
        }
      }
    }
    {
      case Black => blackCount
      case White => whiteCount
    }
  }

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
