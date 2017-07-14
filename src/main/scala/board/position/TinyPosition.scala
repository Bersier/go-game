package board.position

import board.{Color, Size}
import zobristcode.ZCode128

private final class TinyPosition extends PositionInternal {
  override protected[this] def nextPositionBuilder: Builder = ???

  /**
    * @return the size of one side of the square board
    */
  override protected[this] implicit def size: Size = ???

  /**
    * @return the color at the specified intersection
    */
  override def apply(i: Int, j: Int): Color = ???

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ???
}
