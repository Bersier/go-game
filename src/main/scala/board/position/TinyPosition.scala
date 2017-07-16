package board.position

import board.{Color, Intersection, Size}
import commons.Utils
import zobristcode.ZCode128

private final
class TinyPosition private(reference: Position, updates: Long) extends PositionInternal {

  override protected[this] def nextPositionBuilder: Builder = Builder(apply: (Int, Int) => Color)

  override def apply(x: Intersection): Color = {
    updatedIntersections
      .find { case (x2, _) => x == x2 }
      .map { _._2 } getOrElse reference(x)
  }

  override def toZobristCode: ZCode128 = {
    updatedIntersections.foldLeft(reference.toZobristCode) {
      case (zCode128: ZCode128, (x: Intersection, color: Color)) => {
        zCode128 ^ ZobristCoder.get.code128(x, reference(x)) ^ ZobristCoder.get.code128(x, color)
      }
    }
  }

  override protected[position] implicit def size: Size = reference.size

  private[this] def updatedIntersections = {
    val sizeBits = Utils.intLog(size - 1)
    val moveBits = 2*sizeBits + 2
    val coordinateMask = (1 << sizeBits) - 1
    for (b <- 0 to 64 - moveBits by moveBits) yield {
      val i = (updates >>> b) & coordinateMask
      val j = (updates >>> (b + sizeBits)) & coordinateMask
      val colorLong = (updates >>> (b + 2*sizeBits)) & 3
      (Intersection(i.toInt, j.toInt), Color.fromInt(colorLong.toInt))
    }
  }
}
