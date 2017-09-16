package board.position
import board.{Dihedral4, Move, PlayerColor}
import commons.Utils

final class CPW private(private val position: Position) extends AnyVal with Position {
  /**
    * The order of the returned positions is randomized (but not uniformly over all permutations).
    *
    * @param player    for which the next positions shall be returned
    * @param forbidden usually, the previous positions, including the current one
    * @return all the possible next positions, after a non-pass move of the player
    */
  override def nextPositions(player: PlayerColor)(implicit forbidden: Set[Position]) = {
    position.nextPositions(player)(Utils.mockSet.toSet)
  }

  /**
    * The order of the returned positions is randomized (but not uniformly over all permutations).
    *
    * @param player             for which the next positions shall be returned
    * @param forbiddenCanonical usually, the previous positions, including the current one; should
    *                           be given in canonical form
    * @return all the possible next positions, up to isomorphism, after a non-pass move of the
    *         player
    */
  override def nextCanonicalPositions(player: PlayerColor)(implicit forbiddenCanonical: Set[Position]) = ???

  /**
    * Requires that the passed move be legal.
    *
    * @param move   to be made
    * @param player who's playing
    * @param prev   the positions already seen
    * @return the position resulting from the given move
    */
  override def withMove(move: Move, player: PlayerColor)(implicit prev: Set[Position]) = ???

  /**
    * @return an equivalent position to this one, but rotated or reflected according to the given
    *         transformation
    */
  override def after(transformation: Dihedral4) = ???

  /**
    * @return a 128 bit long hash code for this position
    */
  override def toZobristCode = ???

  /**
    * @return how many stones of each color are on the board in this position
    */
  override def count = ???

  /**
    * @return how much area each player controls
    */
  override def area = ???
}
