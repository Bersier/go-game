package board.position
import board.{Color, Intersection, Size}
import zobristcode.ZCode128

private final
class EfficientPosition private(representation: Array[Long], zc1: Long, zc2: Long)
  extends ZCodeCacher(zc1, zc2) {

  private[this] def this(representation: Array[Long], zCode128: ZCode128) = {
    this(representation, zCode128._1, zCode128._2)
  }

  def this(map: (Int, Int) => Color)(implicit size: Size) = {
    this(EfficientPosition.toArray(map), ZobristCoder.get.computeCode(map))
  }

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

  protected[this] override def verticalFlip(implicit size: Size): Unit = {
    for (i <- 0 until size / 2) {
      val temp = representation(i)
      representation(i) = representation(size - 1 - i)
      representation(size - 1 - i) = temp
    }
    val code = ZobristCoder.get.computeCode((i, j) => this(i, j))
    zCode1 = code._1
    zCode2 = code._2
  }
}

private object EfficientPosition {

  private def toArray(map: (Int, Int) => Color)(implicit size: Size): Array[Long] = {
    for (i <- 0 until size) yield {
      var row = 0L
      for (j <- (size - 1) to 0 by -1) {
        row <<= 2
        row |= map(i, j).toInt
      }
      row
    }
  }.toArray
}