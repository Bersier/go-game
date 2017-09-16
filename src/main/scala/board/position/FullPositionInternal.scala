package board.position

import board.Dihedral4

protected trait FullPositionInternal extends PositionInternal[FullPosition] with FullPosition {

  override def after(permutation: Dihedral4): Position = Position((i, j) => this(permutation(i, j)))
}
