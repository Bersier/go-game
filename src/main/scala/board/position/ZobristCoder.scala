package board.position

import board.{Color, Intersection, Size}
import main.Config
import zobristcode.{ZCode128, ZobristBase64}

private class ZobristCoder(size: Size) {

  private[this] val zobristBase1 = new ZobristBase64(l)
  private[this] val zobristBase2 = new ZobristBase64(l)

  private[this] def l: Int = size * size * 3

  def computeCode(color: (Int, Int) => Color)(implicit size: Size): ZCode128 = {
    var code1: Long = 0
    var code2: Long = 0
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        code1 ^= codeI(i, j, color(i, j), zobristBase1)
        code2 ^= codeI(i, j, color(i, j), zobristBase2)
      }
    }
    ZCode128(code1, code2)
  }

  @inline def code128(x: Intersection, color: Color)(implicit size: Size): ZCode128 = {
    ZCode128(code1(x, color), code2(x, color))
  }

  @inline def code1(x: Intersection, color: Color)(implicit size: Size): Long = {
    codeI(x.i, x.j, color, zobristBase1)
  }
  @inline def code2(x: Intersection, color: Color)(implicit size: Size): Long = {
    codeI(x.i, x.j, color, zobristBase2)
  }

  @inline private[this] def codeI(i: Int, j: Int, color: Color, zobristBase: ZobristBase64)
                                (implicit size: Size): Long = {
    zobristBase(3 * (size * i + j) + color.toInt)
  }
}

private object ZobristCoder {
  val get: ZobristCoder = new ZobristCoder(Config.maxSize)
}