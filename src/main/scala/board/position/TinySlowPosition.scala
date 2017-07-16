package board.position

import board.{Color, Intersection, Size}
import commons.Memoizer
import main.Config
import zobristcode.ZCode128

private final
class TinySlowPosition private(reference: Position, updates: Long) extends PositionInternal {

  override protected[this] def nextPositionBuilder: Builder = Builder(apply: (Int, Int) => Color)

  override def apply(x: Intersection): Color = {
    TinySlowPosition.moveEncoder.decode(updates).getOrElse(x, reference(x))
  }

  override def toZobristCode: ZCode128 = {
    TinySlowPosition.moveEncoder.decode(updates).foldLeft(reference.toZobristCode) {
      case (zCode128: ZCode128, (x: Intersection, color: Color)) => {
        zCode128 ^ ZobristCoder.get.code128(x, reference(x)) ^ ZobristCoder.get.code128(x, color)
      }
    }
  }

  override protected[position] implicit def size: Size = reference.size
}

private object TinySlowPosition {
  private val moveEncoderMem = Memoizer((i: Int) => new MoveEncoder()(Size(i)))(Config.maxSize)

  def moveEncoder(implicit size: Size): MoveEncoder = moveEncoderMem(size)
}