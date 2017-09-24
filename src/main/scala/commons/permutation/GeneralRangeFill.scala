package commons.permutation

import scala.util.Random

private final
class GeneralRangeFill private(left: LeftRangeFill,
                               right: RangeFill,
                               private[this] var count: Int,
                               override val size: Int) extends LeftRangeFill with Permutation {

  override def apply(i: Int): Int = {
    count -= 1
    if (i < left.freeCount) left(i)
    else right(i - left.freeCount) + left.size
  }

  override def freeCount: Int = count

  override def hasNext: Boolean = count > 0

  override def next: Int = this(Random.nextInt(freeCount))
}
