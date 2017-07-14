package zobristcode

import commons.Utils.{rotate, secureRandom}

final class ZobristBase128(size: Int) {
  private[this] val mem: Array[Long] = Array.fill(2*((size - 1)/64 + 1))(secureRandom.nextLong)

  def apply(i: Int): ZCode128 = {
    ZCode128(rotate(mem(i/64*2), i % 64), rotate(mem(i/64*2 + 1), i % 64))
  }
}
