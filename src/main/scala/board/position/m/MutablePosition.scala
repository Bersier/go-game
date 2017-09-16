package board.position.m

import board.Move
import board.position.Position

trait MutablePosition extends Position {

  /**
    * Requires the given move to be legal.
    */
  def play(move: Move): this.type

  def toUnmodifiable: Position
}
