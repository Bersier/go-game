package board.position

import board.{Color, Intersection}
import zobristcode.ZCode128

protected abstract
class ZCodeCacher[+P <: Position](protected[this] var zCode1: Long, protected[this] var zCode2: Long)
  extends PositionInternal[P] with Builder[P] {
  this: P =>

  final override def toZobristCode: ZCode128 = ZCode128(zCode1, zCode2)

  final override def hashCode: Int = zCode2.toInt

  protected[this] final override def update(x: Intersection, color: Color): Unit = {
    updateZCode(x, this(x), color)
    updateRaw(x, color)
  }

  protected[this] def updateRaw(x: Intersection, color: Color): Unit

  protected[this] override
  def updateZCode(x: Intersection, oldColor: Color, newColor: Color): Unit = {
    zCode1 ^= ZobristCoder.get.code1(x, oldColor) ^ ZobristCoder.get.code1(x, newColor)
    zCode2 ^= ZobristCoder.get.code2(x, oldColor) ^ ZobristCoder.get.code2(x, newColor)
  }

  protected[this] override def updateZCode(code: ZCode128): Unit = {
    zCode1 = code._1
    zCode2 = code._2
  }
}
