package main

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MagicSquare extends App {

  val stepSize = 100000
  var bound = 6600000
  while (true) {
    findCandidates(bound, bound + stepSize)
    bound += stepSize
    println("Upper bound = " + bound)
    println("\n\n")
  }

  def findCandidates(lowerBound: Int, upperBound: Int): Unit = {
    val map = mutable.Map.empty[Int, ArrayBuffer[Array[Int]]]
    val iLowerBound = math.max(2, math.sqrt(lowerBound.toDouble / 3).ceil.toInt)
    for (i <- iLowerBound until math.sqrt(upperBound - 1).toInt + 1) {
      val i2 = i * i
      val jBound = math.min(i, math.sqrt(upperBound - i2 - 1).toInt + 1)
      val jLowerBound = math.max(1, math.sqrt((lowerBound.toDouble - i2) / 2).ceil.toInt)
//      println(s"i = $i")
//      println(s"jBound = $jBound")
      for (j <- jLowerBound until jBound) {
        val j2 = j * j
        val kBound = math.min(j, math.sqrt(upperBound - i2 - j2 - 1).toInt + 1)
//        println(s"j = $j")
//        println(s"kBound = $kBound")
        for (k <- math.max(0, math.sqrt(lowerBound - i2 - j2).ceil.toInt) until kBound) {
          map.getOrElseUpdate(i2 + j2 + k*k, ArrayBuffer.empty[Array[Int]]) += Array(i, j, k)
        }
      }
    }
    println("map.size = " + map.size)
    val c2 = map.values.map { loop }.filter(_.size >= 8)
    println("c2.size = " + c2.size)
    val c3 = c2.filter(cruxFilter)
    println("c3.size = " + c3.size)
    for (arrays <- c3) {
      val sum = arrays(0).map(i => i*i).sum
      val iCount = getICount(arrays)
      val possibleCruxes = iCount.filter{ case (_, count) => count >= 4 }.keys
      for (crux <- possibleCruxes) {
        val possibleDiagonals = arrays.filter(_ contains crux)
        for (diagonal <- possibleDiagonals) {
          val topLeftAndBottomRight = diagonal.filterNot(_ == crux)
          val topLeft = topLeftAndBottomRight(0)
          val bottomRight = topLeftAndBottomRight(1)
          val topLeftLines = arrays
            .filter(_.contains(topLeft))
            .filterNot(_ contains bottomRight)
            .filterNot(_ contains crux)
          if (topLeftLines.size >= 2) {
            for (topRow <- topLeftLines) {
              val topRowExceptLeft = topRow.filterNot(_ == topLeft)
              for (topCenter <- topRowExceptLeft) {
                val topRight = topRowExceptLeft.filterNot(_ == topCenter)(0)
                val topRight2 = topRight * topRight
                val bottomRight2 = bottomRight * bottomRight
                for (middleRight <- missing(topRight2, bottomRight2, sum)) {
                  val crux2 = crux * crux
                  for (middleLeft <- missing(crux2, middleRight*middleRight, sum)) {
                    for (bottomLeft <- missing(topLeft * topLeft, middleLeft*middleLeft, sum)) {
                      val bottomLeft2 = bottomLeft * bottomLeft
                      if (bottomLeft2 + crux2 + topRight2 == sum) {
                        for (bottomCenter <- missing(bottomLeft2, bottomRight2, sum)) {
                          if (bottomCenter*bottomCenter + crux2 + topCenter * topCenter == sum) {
                            println(s"|$topLeft|$topCenter|$topRight")
                            println(s"|$middleLeft|$crux|$middleRight")
                            println(s"|$bottomLeft|$bottomCenter|$bottomRight\n")
                            System.exit(0)
                          }
                        }
                      }
                    }
                  }
                }
//                val possibleCenterColumns = arrays
//                  .filter(_ contains crux)
//                  .filterNot(_ contains bottomRight)
//                for (centerColumn <- possibleCenterColumns.filter(_ contains topCenter)) {
//                  val bottomCenter = centerColumn.filterNot(_ == crux).filterNot(_ == topCenter)(0)
//                  val topRight = topRowExceptLeft.filterNot(_ == topCenter)(0)
//                  val middleRight2 = sum - topRight*topRight - bottomRight*bottomRight
//                  val middleRight = math.sqrt(middleRight2).toInt
//                  if (middleRight*middleRight == middleRight2) {
//                    val middleLeft2 = sum - crux*crux - middleRight2
//                    val middleLeft = math.sqrt(middleLeft2).toInt
//                    if (middleLeft*middleLeft == middleLeft2) {
//                      val bottomLeft2 = sum - bottomCenter*bottomCenter - bottomRight*bottomRight
//                      val bottomLeft = math.sqrt(bottomLeft2).toInt
//                      if (bottomLeft*bottomLeft == bottomLeft2) {
//                        if (topLeft*topLeft + middleLeft2 + bottomLeft2 == sum &&
//                            bottomLeft2 + crux*crux + topRight*topRight == sum) {
//                          println(s"|$topLeft|$topCenter|$topRight")
//                          println(s"|$middleLeft|$crux|$middleRight")
//                          println(s"|$bottomLeft|$bottomCenter|$bottomRight\n")
//                          System.exit(0)
//                        }
//                      }
//                    }
//                  }
//                }
              }
            }
          }
        }
      }
    }
  }

  def missing(other1Squared: Int, other2Squared: Int, sum: Int): Option[Int] = {
    val missingSquared = sum - other1Squared - other2Squared
    val missingCandidate = math.sqrt(missingSquared).toInt
    if (missingCandidate * missingCandidate == missingSquared) Some(missingCandidate)
    else None
  }

  def cruxFilter(arrays: IndexedSeq[Array[Int]]): Boolean = {
    getICount(arrays).values.exists(_ >= 4)
  }

  def loop(arrays: IndexedSeq[Array[Int]]): IndexedSeq[Array[Int]] = {
    val iCount = getICount(arrays)
    val nextArrays = arrays.filter {
      array => array.forall { i => iCount(i) >= 2 }
    }
    if (nextArrays.size < 8) nextArrays
    else if (nextArrays.size == arrays.size) nextArrays
    else loop(nextArrays)
  }

  private def getICount(arrays: IndexedSeq[Array[Int]]) = {
    val flattened = arrays.flatten
    val iCount = mutable.Map.empty[Int, Int]
    for (i <- flattened) {
      iCount(i) = iCount.getOrElse(i, 0) + 1
    }
    iCount
  }
}
