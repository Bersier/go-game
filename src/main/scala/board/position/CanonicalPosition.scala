package board.position

import board.{Move, PlayerColor}
import collection.Set
import zobristcode.ZCode128

trait CanonicalPosition extends Position {

  override def nextPositions(player: PlayerColor)
                            (implicit forbidden: Set[ZCode128]): Iterator[CanonicalPosition]

  override def withMove(move: Move, player: PlayerColor)
                       (implicit prev: Set[ZCode128]): CanonicalPosition

  def toFull: FullPosition
}
