package zobristcode

/**
  * Careful, mutable!
  */
private final class ZCode128Impl(l1: Long, l2: Long) extends ZCode128 {
  override val _1: Long = l1
  override val _2: Long = l2
}
