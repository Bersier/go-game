package board.position

import board.{Color, Intersection, Size}
import commons.Memoizer
import main.Config
import zobristcode.{ZCode128, ZobristBase64}

class ZobristCoder(size: Size) {

  private[this] val l: Int = size * size * 3

  private[this] val zobristBase1 = new ZobristBase64(l)
  private[this] val zobristBase2 = new ZobristBase64(l)

  private[position] def computeCode(color: (Int, Int) => Color)(implicit size: Size): ZCode128 = {
    var code1: Long = 0
    var code2: Long = 0
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        code1 ^= code(i, j, color(i, j), zobristBase1)
        code2 ^= code(i, j, color(i, j), zobristBase2)
      }
    }
    ZCode128(code1, code2)
  }

  @inline private[position] def code1(x: Intersection, color: Color)(implicit size: Size): Long = {
    code(x.i, x.j, color, zobristBase1)
  }
  @inline private[position] def code2(x: Intersection, color: Color)(implicit size: Size): Long = {
    code(x.i, x.j, color, zobristBase2)
  }

  @inline private[this] def code(i: Int, j: Int, color: Color, zobristBase: ZobristBase64)
                                (implicit size: Size): Long = {
    zobristBase(3 * (size * i + j) + color.toInt)
  }
}

object ZobristCoder {
  private val zobristCoderMem = Memoizer((i: Int) => new ZobristCoder(Size(i)))(Config.maxSize)

  def apply(implicit size: Size): ZobristCoder = zobristCoderMem(size)
}