package board.position

import board._
import collection.Set
import commons.Utils
import zobristcode.ZCode128

/**
  * Represents a position in a game of Go. Immutable.
  *
  * Extend PositionInternal instead of this trait.
  */
trait Position extends AbstractPosition {

  /**
    * The order of the returned positions is randomized (but not uniformly over all permutations).
    *
    * @param player for which the next positions shall be returned
    * @param forbidden usually, the previous positions, including the current one
    * @return all the possible next positions, after a non-pass move of the player
    */
  def nextPositions(player: PlayerColor)(implicit forbidden: Set[ZCode128]): Iterator[Position]

  /**
    * Requires that the passed move be legal.
    *
    * @param move to be made
    * @param player who's playing
    * @param prev the positions already seen
    * @return the position resulting from the given move
    */
  def withMove(move: Move, player: PlayerColor)(implicit prev: Set[ZCode128]): Position

  /**
    * @return a 128 bit long hash code for this position
    */
  def toZobristCode: ZCode128

  /**
    * @return how many stones of each color are on the board in this position
    */
  def count: Color => Int

  /**
    * @return how much area each player controls
    */
  def area: PlayerColor => Int
}

object Position {

  def initial(implicit size: Size): Position = Builder.initial.build

  def apply(stateDescription: (Int, Int) => Color)(implicit size: Size): Position = {
    Builder(stateDescription).build
  }

  /**
    * The order of the returned intersections is randomized (but not uniformly over all
    * permutations).
    *
    * @return all the intersections of a board of the given size
    */
  def intersections(implicit size: Size): Iterator[Intersection] = {
    for (k <- Utils.cheapShuffledRange(size * size).iterator) yield {
      Intersection(k / size, k % size)
    }
  }

  /**
    * @return all the intersections of a board of the given size
    */
  def orderedIntersections(implicit size: Size): Iterator[Intersection] = {
    for (i <- (0 until size).iterator; j <- 0 until size) yield {
      Intersection(i, j)
    }
  }
}

