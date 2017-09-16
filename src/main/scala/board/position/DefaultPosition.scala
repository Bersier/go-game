package board.position

import board.{Color, Intersection, Size}
import zobristcode.ZCode128

private final class DefaultPosition private(board: Array[Array[Color]], zc1: Long, zc2: Long)
  extends ZCodeCacher(zc1, zc2) {

  private[this] def this(board: Array[Array[Color]], zCode: ZCode128) = {
    this(board, zCode._1, zCode._2)
  }

  def this(board: Array[Array[Color]]) = {
    this(board, ZobristCoder.get.computeCode((i, j) => board(i)(j))(Size(board.length)))
  }

  def this(map: (Int, Int) => Color)(implicit size: Size) = this(Array.tabulate(size, size)(map))

  def this(position: PositionInternal[Position]) = this((i, j) => position(i, j))(position.size)

  override def apply(i: Int, j: Int): Color = board(i)(j)

  override implicit def size: Size = Size(board.length)

  override protected[this] def nextPositionBuilder: Builder[Position] = {
    new DefaultPosition(board map (_.clone()), zCode1, zCode2)
  }

  override protected[this] def updateRaw(x: Intersection, color: Color): Unit = {
    board(x.i)(x.j) = color
  }

  override def build: this.type = this
}
