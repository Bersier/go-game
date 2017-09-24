package commons.permutation

private trait LeftRangeFill extends RangeFill {

  def apply(i: Int): Int

  def freeCount: Int

  def size: Int
}
