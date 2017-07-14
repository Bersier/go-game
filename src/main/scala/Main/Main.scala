package Main

import commons.Utils

object Main extends App {

  println(((-1.0)/64).floor)

  for (i <- 1 to 10) {
    for (j <- Utils.cheapShuffledRange(10)) {
      print(j + " ")
    }
    println
  }
  println

  for (i <- 1 to 10) {
    for (j <- Utils.shuffledRange(10.toShort)) {
      print(j + " ")
    }
    println
  }
}
