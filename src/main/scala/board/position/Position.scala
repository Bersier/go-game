package board.position

import board._
import zobristcode.ZCode128

/**
  * Represents a position in a game of Go. Immutable.
  */
trait Position {

  /**
    * @return the color at the specified intersection
    */
  def apply(i: Int, j: Int): Color = apply(Intersection(i, j))

  /**
    * @return the color at the given intersection
    */
  def apply(point: Intersection): Color = apply(point.i, point.j)

  /**
    * The order of the returned positions is randomized (but not uniformly over all permutations).
    *
    * @param player for which the next positions shall be returned
    * @param forbidden usually, the previous positions, including the current one
    * @return all the possible next positions, after a non-pass move of the player
    */
  def nextPositions(player: ProperColor)(implicit forbidden: Set[Position]): Iterator[Position]

  /**
    * Requires that the passed move be legal.
    *
    * @param move to be made
    * @param player who's playing
    * @param prev the positions already seen
    * @return the position resulting from the given move
    */
  def withMove(move: Move, player: ProperColor)(implicit prev: Set[Position]): Position

  /**
    * @return a 128 bit long hash code for this position
    */
  def toZobristCode: ZCode128
}

object Position {

  def initial: Position = new DefaultPosition()
}

