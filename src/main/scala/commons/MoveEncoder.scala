package commons

import board.{Color, Intersection, Size}

final class MoveEncoder(size: Size) {

  private[this] val length = 64 * math.log(2) / (math.log(3) + 2 * math.log(size: Int)) toInt

  def encode(moves: Seq[(Intersection, Color)]): Long = {
    assert(moves.length == length)
    val pieces = moves.flatMap(move => Seq(move._1.i, move._1.j, move._2.toInt))
    LongEncoder.encode(pieces, Bounds)
  }

  def decode(encoded: Long): Map[Intersection, Color] = {
    LongEncoder.decode(encoded, Bounds)(3*length).sliding(3, 3)
      .map(triple => (Intersection(triple(0), triple(1)), Color.fromInt(triple(2))))
      .toMap
  }

  private object Bounds extends IndexedSeq[Int] {
    override val length: Int = Int.MaxValue

    override def apply(idx: Int): Int = idx % 3 match {
      case 2 => 3
      case _ => size
    }

    override def iterator: Iterator[Int] = new Iterator[Int] {
      private[this] var i = -1

      override def hasNext: Boolean = true

      override def next(): Int = {
        i += 1
        Bounds(i)
      }
    }
  }
}
