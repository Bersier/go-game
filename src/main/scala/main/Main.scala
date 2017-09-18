package main

import board.position.Position
import board.{Black, PlayerColor, Size}
import zobristcode.ZCode128

import scala.collection.mutable

object Main extends App {

  var cleanupTime: Long = 0
  var playAtTime: Long = 0
  var canonifyTime: Long = 0
  var maxIteratorTime: Long = 0
  private var startTime: Long = 0

  type Positions = mutable.Set[ZCode128]

  for (i <- 1 until 20) {
    println("Komi for " + i + "x" + i + ": " + komi(Size(i)))
  }

  def komi(implicit size: Size): Int = {
    callCount = 0
    cleanupTime = 0
    startTime = System.currentTimeMillis()
    val maxKomi = size * size
    komi(-maxKomi, maxKomi, Black, Position.initial)(mutable.Set.empty[ZCode128], size)
  }

  private
  def komi(min: Int, max: Int, color: PlayerColor, position: Position)
          (implicit prev: Positions, size: Size): Int = withUpdatedPrev(position) {
    komiBody(min, -komiPass(-max, -min, color.dual, position), max, color, position)
  }

  private
  def komiPass(min: Int, max: Int, color: PlayerColor, position: Position)
              (implicit prev: Positions, size: Size): Int = {
    komiBody(min, areaDiff(position, color), max, color, position)
  }

  private
  def komiBody(min: Int, newMin: /*=> */Int, max: Int, color: PlayerColor, position: Position)
              (implicit prev: Positions, size: Size): Int = {
    if (callCount % 100000 == 0) {
      println("callcount = " + callCount)
      println("cleanupTime = " + cleanupTime)
      println("playAtTime = " + playAtTime)
      println("canonifyTime = " + canonifyTime)
      println("maxIteratorTime = " + maxIteratorTime)
      println("totalTime = " + (System.currentTimeMillis() - startTime) + "\n")
    }
    callCount += 1
//    print(position)
//    println(color)
    val nextPositions = position.nextPositions(color)(prev)

    def loop(min: Int): Int = {
      if (min >= max || nextPositions.isEmpty) min
      else loop(math.max(min, -komi(-max, -min, color.dual, nextPositions.next)))
    }

    val a = loop(math.max(min, newMin))
//    println(">")
    a
  }

  private def areaDiff(position: Position, color: PlayerColor) = {
    val area = position.area
//    println(color + " " + (area(color) - area(color.dual)))
    area(color) - area(color.dual)
  }

  private var maxPrevCount = 0
  private var callCount = 0

  private
  def withUpdatedPrev[A](position: Position)(getA: => A)(implicit prev: Positions): A = {
    prev += position.toZobristCode
    val a = getA
    prev -= position.toZobristCode
    a
  }
}
