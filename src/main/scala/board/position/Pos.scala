package board.position

import board.{Black, Color, Intersection, Move, None, Pass, ProperColor, Size, White}
import commons.Utils
import zobristcode.{ZCode128, ZobristBase64}

import scala.collection.mutable

private trait Pos[PosT <: Pos[PosT]] extends Position {
  this: PosT =>

  final override
  def nextPositions(player: ProperColor)(implicit forbidden: Set[Position]): Iterator[PosT] = {
    for (x <- this.intersections; n <- withMove(x, player)) yield n
  }

  final override
  def withMove(move: Move, player: ProperColor)(implicit prev: Set[Position]): PosT = move match {
    case x: Intersection => {
      require(apply(x) == None, s"Illegal move: $x is already occupied")
      require(prev(this), "'previous' should contain the current position")
      val result: PosT = copy
      result.update(x, player)
      result.cleanup(x, player)
      require(!prev(result), s"Illegal move: results in a previous position: $result")
      result
    }
    case Pass => {
      this
    }
  }

  protected[this] def copy: PosT

  /**
    * Sets the color at x (and updates the hashCode and zobristCode accordingly).
    */
  private def update(x: Intersection, color: Color): Unit = {
    updateZCode(x, this(x), color)
    updateRaw(x, color)
  }

  protected[this] def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit

  /**
    * Used in the implementation of 'update'. Only sets the given intersection to the given color.
    * In particular, doesn't worry about updating the hashCode or zobristCode.
    */
  protected[this] def updateRaw(implicit x: Intersection, color: Color): Unit

  private[this] def intersections: Iterator[Intersection] = {
    for (k <- Utils.cheapShuffledRange(size * size).iterator) yield {
      Intersection((k / size).toByte, (k % size).toByte)
    }
  }

  private[this] def withMove(x: Intersection, color: ProperColor)
                            (implicit forbidden: Set[Position]): Option[PosT] = this(x) match {
    case None => {
      val result: PosT = copy
      result.update(x, color)
      result.cleanup(x, color)
      if (result == this || forbidden(result)) Option.empty
      else Some(result)
    }
    case _ => Option.empty
  }

  private def cleanup(x: Intersection, color: ProperColor) {
    def removeDead(x: Intersection, color: ProperColor)(alive: mutable.Set[Intersection]) {
      val visited = mutable.Set[Intersection]()
      def isAlive(intersection: Intersection): Boolean = {
        if (alive(intersection)) true
        else if (visited(intersection)) false
        else if (this (intersection) == None) true
        else if (this (intersection) != color) false
        else {
          visited += intersection
          intersection.neighbors(size).exists(stone => isAlive(stone))
        }
      }

      if (isAlive(x)) {
        alive ++= visited
      } else {
        for (x <- visited) {
          update(x, None)
        }
      }
    }

    val transAlives = mutable.Set[Intersection]()
    for (n <- x.neighbors(size) if this(n) == color.dual) {
      removeDead(n, color.dual)(transAlives)
    }

    removeDead(x, color)(Utils.MockSet)
  }


  /**
    * @return the size of one side of the square board
    */
  protected[this] implicit def size: Size

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

object Pos {

  private[position]
  def computeZobristCode(color: (Int, Int) => Color)(implicit size: Size): ZCode128 = {
    var code1: Long = 0
    var code2: Long = 0
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        code1 ^= xCode(i, j, color(i, j), zobristBase1)
        code2 ^= xCode(i, j, color(i, j), zobristBase2)
      }
    }
    ZCode128(code1, code2)
  }

  @inline private[position] def xCode1(x: Intersection, color: Color)(implicit size: Size): Long = {
    xCode(x.i, x.j, color, zobristBase1)
  }
  @inline private[position] def xCode2(x: Intersection, color: Color)(implicit size: Size): Long = {
    xCode(x.i, x.j, color, zobristBase2)
  }

  @inline private[this] def xCode(i: Int, j: Int, color: Color, zobristBase: ZobristBase64)
                                 (implicit size: Size): Long = {
    val a = color match {
      case None => 0
      case White => 1
      case Black => 2
    }
    zobristBase(3 * (size * i + j) + a)
  }

  private[this] val maxSize: Int = 19

  private[this] val zobristBase1 = new ZobristBase64(maxSize * maxSize * 3)
  private[this] val zobristBase2 = new ZobristBase64(maxSize * maxSize * 3)
}
