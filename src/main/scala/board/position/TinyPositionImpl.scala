package board.position

import board.{Color, Intersection, Size}
import zobristcode.ZCode128

private final class TinyPositionImpl extends Pos[TinyPositionImpl] {

  override protected[this] def copy: TinyPositionImpl = ???

  /**
    * Used in the implementation of 'update'. Only sets the given intersection to the given color.
    * In particular, doesn't worry about updating the hashCode or zobristCode.
    */
  override protected[this] def updateRaw(implicit x: Intersection, color: Color): Unit = ???

  /**
    * @return the size of one side of the square board
    */
  override protected[this] implicit def size: Size = ???

  /**
    * @return the color at the specified intersection
    */
  override def apply(i: Int, j: Int): Color = ???

  override protected[this] def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit = ???

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ???
}
