package board.position

import board.{Dihedral4, Move, PlayerColor}
import zobristcode.ZCode128

import collection.Set

trait FullPosition extends Position {

  /**
    * @return an equivalent position to this one, but rotated or reflected according to the given
    *         transformation
    */
  def after(permutation: Dihedral4): Position

  override def nextPositions(player: PlayerColor)
                            (implicit forbidden: Set[ZCode128]): Iterator[FullPosition]

  override def withMove(move: Move, player: PlayerColor)
                       (implicit prev: Set[ZCode128]): FullPosition

  def toCanonical: CanonicalPosition
}
