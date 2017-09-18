package board.position

import board.{Color, Size}
import zobristcode.ZCode128

private final
class EfficientCanonicalPosition private(representation: Array[Long], zc1: Long, zc2: Long)
  extends EfficientPosition[CanonicalPosition](representation, zc1, zc2)
    with CanonicalPositionInternal {

  private[this] def this(representation: Array[Long], zCode128: ZCode128) = {
    this(representation, zCode128._1, zCode128._2)
  }

  def this(map: (Int, Int) => Color)(implicit size: Size) = {
    this(EfficientPosition.toArray(map), ZobristCoder.get.computeCode(map))
  }

  override def build: this.type = canonify

  override def toFull: FullPosition = ???

  override protected[this] def nextPositionBuilder: Builder[CanonicalPosition] = {
    new EfficientCanonicalPosition(representation.clone, zCode1, zCode2)
  }
}
