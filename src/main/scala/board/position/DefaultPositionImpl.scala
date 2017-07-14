package board.position

import board.{Color, Intersection, None, Size}
import zobristcode.ZCode128

private final class DefaultPositionImpl private(board: Array[Array[Color]],
                                                private[this] var vCode1: Long,
                                                private[this] var vCode2: Long) extends Pos[DefaultPositionImpl] {

  private[this] def this(size: Size, zCode: ZCode128) = {
    this(Array.fill[Color](size, size)(None), zCode._1, zCode._2)
  }

  //noinspection UnnecessaryPartialFunction
  def this(size: Size) = {
    this(size, Pos.computeZobristCode{ case _ => None }(size))
  }

  override def size: Size = Size(board.length)

  override def apply(i: Int, j: Int): Color = board(i)(j)

  override def copy: DefaultPositionImpl = {
    new DefaultPositionImpl(board map (_.clone()), zCode1, zCode2)
  }

  override protected def updateRaw(implicit x: Intersection, color: Color): Unit = {
    board(x.i)(x.j) = color
  }

  override protected[this]
  def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit = {
    vCode1 ^= Pos.xCode1(x, oldColor)(size) ^ Pos.xCode1(x, newColor)(size)
    vCode2 ^= Pos.xCode2(x, oldColor)(size) ^ Pos.xCode2(x, newColor)(size)
  }

  @inline private[this] def zCode1: Long = vCode1
  @inline private[this] def zCode2: Long = vCode2

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ZCode128(zCode1, zCode2)

  override def hashCode: Int = zCode2.toInt
}
