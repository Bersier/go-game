package board.position

import board.{Color, Intersection, None, Size}
import zobristcode.ZCode128

private final class DefaultPosition private(board: Array[Array[Color]],
                                            private[this] var zCode1: Long,
                                            private[this] var zCode2: Long)
  extends PositionInternal with Builder {

  private[this] def this(board: Array[Array[Color]], zCode: ZCode128) = {
    this(board, zCode._1, zCode._2)
  }

  //noinspection UnnecessaryPartialFunction
  def this()(implicit size: Size) = {
    this(Array.fill[Color](size, size)(None), ZobristCoder.get.computeCode{ case _ => None })
  }

  def this(board: Array[Array[Color]])(implicit size: Size) = {
    this(board, ZobristCoder.get.computeCode{ (i, j) => board(i)(j) })
  }

  def this(map: (Int, Int) => Color)(implicit size: Size) = {
    this(Array.tabulate(size, size)(map))
  }

  override def apply(i: Int, j: Int): Color = board(i)(j)

  override implicit def size: Size = Size(board.length)

  override def nextPositionBuilder: Builder = {
    new DefaultPosition(board map (_.clone()), zCode1, zCode2)
  }

  override def toZobristCode: ZCode128 = ZCode128(zCode1, zCode2)

  override def hashCode: Int = zCode2.toInt

  override def update(x: Intersection, color: Color): Unit = {
    updateZCode(x, this(x), color)
    board(x.i)(x.j) = color
  }

  override def build: Position = this

  private[this] def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit = {
    zCode1 ^= ZobristCoder.get.code1(x, oldColor) ^ ZobristCoder.get.code1(x, newColor)
    zCode2 ^= ZobristCoder.get.code2(x, oldColor) ^ ZobristCoder.get.code2(x, newColor)
  }
}