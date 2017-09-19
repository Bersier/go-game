package board.position

import board.PlayerColor
import zobristcode.ZCode128

import scala.collection.{Set, mutable}

trait CanonicalPositionInternal extends PositionInternal[CanonicalPosition] with CanonicalPosition {

  final override
  def nextPositions(player: PlayerColor)
                   (implicit forbidden: Set[ZCode128]): Iterator[CanonicalPosition] = {
    new Iterator[CanonicalPosition] {
      private[this] val seen = mutable.Set.empty[ZCode128]
      private[this] val candidates = CanonicalPositionInternal.super.nextPositions(player)
      private[this] var nextPosition: CanonicalPosition = _

      override def hasNext: Boolean = {
        if (candidates.hasNext) {
          nextPosition = candidates.next()
          if (seen(next.toZobristCode)) hasNext
          else {
            seen += next.toZobristCode
            true
          }
        }
        else false
      }

      override def next: CanonicalPosition = nextPosition
    }
  }
}
