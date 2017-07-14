package board.position

import board.{Color, Intersection, None, Size}
import zobristcode.ZCode128

private final
class DefaultPosition private(board: Array[Array[Color]],
                              private[this] var vCode1: Long,
                              private[this] var vCode2: Long) extends PositionInternal with Builder {

  private[this] def this(size: Size, zCode: ZCode128) = {
    this(Array.fill[Color](size, size)(None), zCode._1, zCode._2)
  }

  //noinspection UnnecessaryPartialFunction
  def this(size: Size) = {
    this(size, ZobristCoder.computeCode{ case _ => None }(size))
  }

  override def size: Size = Size(board.length)

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
    vCode1 ^= ZobristCoder.code1(x, oldColor)(size) ^ ZobristCoder.code1(x, newColor)(size)
    vCode2 ^= ZobristCoder.code2(x, oldColor)(size) ^ ZobristCoder.code2(x, newColor)(size)
  }

  @inline private[this] def zCode1: Long = vCode1
  @inline private[this] def zCode2: Long = vCode2
}
