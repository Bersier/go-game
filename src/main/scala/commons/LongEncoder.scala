package commons

object LongEncoder {

  def encode(pieces: Seq[Int], bounds: Seq[Int]): Long = {
    var result = 0L
    var multiplier = 1L
    for ((p, b) <- pieces.zip(bounds)) {
      assert(p < b)
      result += p*multiplier
      multiplier *= b
    }
    result
  }

  def decode(encoded: Long, bounds: IndexedSeq[Int])(length: Int = bounds.length): Array[Int] = {
    val result = Array.ofDim[Int](bounds.length)
    var current = encoded
    for (i <- 0 until length) {
      result(i) = (current % bounds(i)).toInt
      current /= bounds(i)
    }
    result
  }

  def decode(encoded: Long, index: Int, bounds: Seq[Int]): Int = {
    (encoded / bounds.view.take(index).map(_.toLong).product) % bounds(index) toInt
  }

  def decode(encoded: Long, startIndex: Int, endIndex: Int, bounds: IndexedSeq[Int]): Array[Int] = {
    val result = Array.ofDim[Int](endIndex - startIndex)
    var current = encoded / bounds.view.take(startIndex).map(_.toLong).product
    for (i <- startIndex until endIndex) {
      result(i) = (current % bounds(i)).toInt
      current /= bounds(i)
    }
    result
  }
}
