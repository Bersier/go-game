package board

sealed trait Dihedral4 {

  def apply(i: Int, j: Int)(implicit size: Size): Intersection

  def apply(x: Intersection)(implicit size: Size): Intersection = apply(x.i, x.j)

  def inverse: Dihedral4
}

sealed trait Dihedral4Involution extends Dihedral4 {
  final override def inverse: this.type = this
}

case object Identity extends Dihedral4 {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(i, j)

  override def inverse: Dihedral4 = Identity
}

case object Rotation1 extends Dihedral4 {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(j, size - 1 - i)

  override def inverse: Dihedral4 = Rotation3
}

case object Rotation2  extends Dihedral4Involution {

  override def apply(i: Int, j: Int)
           (implicit size: Size): Intersection = Intersection(size - 1 - i, size - 1 - j)
}

case object Rotation3  extends Dihedral4 {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(size - 1 - j, i)

  override def inverse: Dihedral4 = Rotation1
}

case object VerticalFlip  extends Dihedral4Involution {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(size - 1 - i, j)
}

case object HorizontalFlip  extends Dihedral4Involution {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(i, size - 1 - j)
}

case object TransposeFlip  extends Dihedral4Involution {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(j, i)
}

case object OppositeFlip  extends Dihedral4Involution {

  override def apply(i: Int, j: Int)
                    (implicit size: Size): Intersection = Intersection(size - 1 - j, size - 1 - i)
}

object Dihedral4 {

  val elements: IndexedSeq[Dihedral4] = Array(
    Identity, Rotation1, Rotation2, Rotation3, VerticalFlip, HorizontalFlip, TransposeFlip,
    OppositeFlip
  )
}
