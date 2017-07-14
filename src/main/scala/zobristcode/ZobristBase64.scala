package zobristcode

import commons.Utils.{rotate, secureRandom}

final class ZobristBase64(size: Int) {
  private[this] val mem: Array[Long] = Array.fill((size - 1)/64 + 1)(secureRandom.nextLong)

  def apply(i: Int): Long = rotate(mem(i / 64), i % 64)
}