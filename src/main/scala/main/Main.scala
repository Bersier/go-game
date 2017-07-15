package main

import commons.LongEncoder

object Main extends App {

  simpleTest()

  def simpleTest(): Unit = {
    val pieces: Seq[Int] = Seq(2,7,4,6)
    val bounds: Seq[Int] = Seq(3,10,5,20)
    println(LongEncoder.decode(LongEncoder.encode(pieces, bounds), 0, bounds))
  }
}
