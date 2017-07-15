package board.position

import board.{Color, Intersection, None}
import zobristcode.ZCode128
import main.Config.size

private final class DefaultPosition private(board: Array[Array[Color]],
                                            private[this] var zCode1: Long,
                                            private[this] var zCode2: Long)
  extends PositionInternal with Builder {

  private[this] def this(board: Array[Array[Color]], zCode: ZCode128) = {
    this(board, zCode._1, zCode._2)
  }

  private[this] def this(zCode: ZCode128) = {
    this(Array.fill[Color](size, size)(None), zCode)
  }

  //noinspection UnnecessaryPartialFunction
  def this() = {
    this(ZobristCoder.computeCode{ case _ => None })
  }

  def this(board: Array[Array[Color]]) = {
    this(board, ZobristCoder.computeCode{ (i, j) => board(i)(j) })
  }

  def this(map: (Int, Int) => Color) = {
    this(Array.tabulate(size, size)(map))
  }

  override def apply(i: Int, j: Int): Color = board(i)(j)

  override def nextPositionBuilder: Builder = {
    new DefaultPosition(board map (_.clone()), zCode1, zCode2)
  }

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ZCode128(zCode1, zCode2)

  override def hashCode: Int = zCode2.toInt

  /**
    * Sets the color at x.
    */
  override def update(x: Intersection, color: Color): Unit = {
    updateZCode(x, this(x), color)
    board(x.i)(x.j) = color
  }

  override def build: Position = this

  private[this] def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit = {
    zCode1 ^= ZobristCoder.code1(x, oldColor) ^ ZobristCoder.code1(x, newColor)
    zCode2 ^= ZobristCoder.code2(x, oldColor) ^ ZobristCoder.code2(x, newColor)
  }
}
