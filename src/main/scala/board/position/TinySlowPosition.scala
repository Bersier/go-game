package board.position

import board.{Color, Intersection, Size}
import commons.MoveEncoder
import main.Config.size
import zobristcode.ZCode128

private final
class TinySlowPosition private(reference: Position, updates: Long)
  extends PositionInternal {
  require(size <= 23)

  override protected[this] def nextPositionBuilder: Builder = {
    new DefaultPosition(apply: (Int, Int) => Color)
  }

  override def apply(x: Intersection): Color = {
    TinyPosition.moveEncoder.decode(updates).getOrElse(x, reference(x))
  }

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ???
}

object TinyPosition {
  def moveEncoder: MoveEncoder = new MoveEncoder
}