package board.position

import board.PlayerColor
import zobristcode.ZCode128

import scala.collection.Set

trait CanonicalPositionInternal extends PositionInternal[CanonicalPosition] with CanonicalPosition {

  final override
  def nextPositions(player: PlayerColor)
                   (implicit forbidden: Set[ZCode128]): Iterator[CanonicalPosition] = {
    super.nextPositions(player).toSet.iterator
  }
}
