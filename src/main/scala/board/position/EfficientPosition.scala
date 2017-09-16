package board.position
import board.{Color, Intersection, Size}

private final
class EfficientPosition private(representation: Array[Long], zc1: Long, zc2: Long)
  extends ZCodeCacher(zc1, zc2) {

  override def apply(i: Int, j: Int): Color = {
    Color.fromInt(((representation(i) >>> 2*j) & 3).toInt)
  }

  override protected[position] implicit def size: Size = Size(representation.length)

  override protected[this] def nextPositionBuilder: Builder[Position] = {
    new EfficientPosition(representation.clone, zCode1, zCode2)
  }

  override def build: this.type = this

  override protected[this] def updateRaw(x: Intersection, color: Color): Unit = {
    // See https://graphics.stanford.edu/~seander/bithacks.html#MaskedMerge
    val mask = 3L << 2*x.j
    val update = color.toInt.toLong << 2*x.j
    representation(x.i) ^= ((representation(x.i) ^ update) & mask)
  }
}
