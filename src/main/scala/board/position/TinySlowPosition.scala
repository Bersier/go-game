package board.position

import board.{Color, Intersection, Size}
import commons.{Memoizer, MoveEncoder}
import zobristcode.ZCode128

private final
class TinySlowPosition private(reference: PositionInternal, updates: Long)
  extends PositionInternal {
  require(size <= TinySlowPosition.maxSize)

  override protected[this] def nextPositionBuilder: Builder = {
    new DefaultPosition(apply: (Int, Int) => Color)
  }

  override def apply(x: Intersection): Color = {
    TinySlowPosition.moveEncoder.decode(updates).getOrElse(x, reference(x))
  }

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode: ZCode128 = ???

  override protected[position] implicit def size: Size = reference.size
}

object TinySlowPosition {
  val maxSize = Size(23)
  private val moveEncoderMem = Memoizer((i: Int) => new MoveEncoder(Size(i)))(maxSize)

  def moveEncoder(implicit size: Size): MoveEncoder = moveEncoderMem(size)
}