package zobristcode

/**
  * Careful, mutable!
  */
trait ZCode128 {

  @inline def _1: Long
  @inline def _2: Long

  def copy: ZCode128

  def ^(that: ZCode128)

  /**
    * Mutates this!
    */
  def ^=(that: ZCode128): Unit = {
    xorUpdate_1(that._1)
    xorUpdate_2(that._2)
  }

  /**
    * Mutates this!
    */
  @inline def xorUpdate_1(l1: Long): Unit

  /**
    * Mutates this!
    */
  @inline def xorUpdate_2(l2: Long): Unit

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
