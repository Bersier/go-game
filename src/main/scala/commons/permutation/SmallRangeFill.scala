package commons.permutation

class SmallRangeFill private(override val size: Int) extends LeftRangeFill {
  assert(size <= (1 << 8))

  private[this] var count = size
  private[this] val representation = Array.ofDim[Byte](2*((size - 1) >> 3) + 1)

  override def apply(i: Int): Int = {
    def apply(i: Int, offset: Int, stride: Int, acc: Int): Int = {
      if (offset == 1) {
        ???
      }
      else if (i < representation(offset)) {
        representation(offset) -= 1
        apply(i, offset + 1, stride >> 1, acc)
      }
      else apply(i - representation(offset), offset + stride, stride >> 1, acc + (stride << 2))
    }
    count -= 1
    apply(i, 0, size >> 3, 0)
  }

  override def freeCount: Int = count
}
