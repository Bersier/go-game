package zobristcode

trait ZCode128 {

  @inline def _1: Long
  @inline def _2: Long

  def ^(that: ZCode128): ZCode128 = new ZCode128Impl(this._1 ^ that._1, this._2 ^ that._2)

  override def equals(other: Any): Boolean = other match {
    case that: ZCode128 => this._1 == that._1 && this._2 == that._2
    case _ => false
  }

  override def hashCode(): Int = _2.toInt

  override def toString: String = _1.toHexString + _2.toHexString
}

object ZCode128 {
  def apply(l1: Long, l2: Long): ZCode128 = new ZCode128Impl(l1, l2)
}
