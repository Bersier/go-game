package board.position.m

import board.Dihedral4
import board.position.FullPosition

trait MutableFullPosition extends MutablePosition with FullPosition {

  def applyTransformation(t: Dihedral4): this.type

  override def toCanonical: MutableCanonicalPosition

  override def toUnmodifiable: FullPosition
}
