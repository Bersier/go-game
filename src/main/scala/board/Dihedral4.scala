package board

import board.position.Position

final class Dihedral4 private(private val value: Int) extends AnyVal {

  def apply(i: Int, j: Int)(implicit size: Size): Intersection = {
    var ni = i
    var nj = j
    if ((value >> 1 & 1) == 1) {
      ni = size - 1 - ni
    }
    if ((value >> 2 & 1) == 1) {
      nj = size - 1 - nj
    }
    if ((value & 1) == 0) Intersection(ni, nj)
    else Intersection(nj, ni)
  }

  def apply(x: Intersection)(implicit size: Size): Intersection = apply(x.i, x.j)
}

object Dihedral4 {

  def elements: IndexedSeq[Dihedral4] = new IndexedSeq[Dihedral4] {
    override def length: Int = 8

    override def apply(index: Int): Dihedral4 = {
      assert(index >= 0)
      assert(index < 8)
      new Dihedral4(index)
    }
  }
}