package main

import board.Size

object Config {

  /**
    * 32 was chosen because it's the maximal size for which TinyPosition can still store 5 updates.
    * Also, it doesn't require too large a ZobristCoder.
    */
  val maxSize: Size = Size(32)
}
