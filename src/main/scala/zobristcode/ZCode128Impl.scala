package zobristcode

/**
  * Careful, mutable!
  */
private final
class ZCode128Impl(private[this] var l1: Long, private[this] var l2: Long) extends ZCode128 {

  override def _1: Long = l1
  override def _2: Long = l2

  override def ^(that: ZCode128) = new ZCode128Impl(this._1 ^ that._1, this._2 ^ that._2)

  /**
    * Mutates this!
    */
  override def ^=(that: ZCode128): Unit = {
    this.l1 ^= l1
    this.l2 ^= l2
  }
}
