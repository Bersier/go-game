package board.position.m

import board.position.{CanonicalPosition, FullPosition}

trait MutableCanonicalPosition extends MutablePosition with CanonicalPosition {

  override def toFull: MutableFullPosition

  override def toUnmodifiable: CanonicalPosition
}
