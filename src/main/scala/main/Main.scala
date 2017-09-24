package main

import board.position.Position
import board.{Black, PlayerColor, Size}
import zobristcode.ZCode128

import scala.collection.mutable

object Main extends App {

  var fCount = 0
  var positionCount = 0
  var cleanupTime: Long = 0
  var cleanupCount = 0
  var removeDeadCount = 0
  var withMoveTime: Long = 0
  var playAtTime: Long = 0
  var canonifyTime: Long = 0
  var maxIteratorTime: Long = 0
  var argsMaxTime: Long = 0
  var fTime: Long = 0
  var fakeTime: Long = 0
  var hasNextTime: Long = 0
  private var startTime: Long = 0

  type Positions = mutable.Set[ZCode128]

  val i = 19
  print("Komi for " + i + "x" + i + ": ")
  println(komi(Size(i)))

  def komi(implicit size: Size): Int = {
    fCount = 0
    positionCount = 0
    callCount = 0
    cleanupTime = 0
    cleanupCount = 0
    removeDeadCount = 0
    withMoveTime = 0
    playAtTime = 0
    canonifyTime = 0
    maxIteratorTime = 0
    argsMaxTime = 0
    fTime = 0
    fakeTime = 0
    hasNextTime = 0
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
      println("callCount = " + callCount)
      println("positionCount = " + positionCount)
      println("cleanupTime = " + cleanupTime)
      println("cleanupCount = " + cleanupCount)
      println("removeDeadCount = " + removeDeadCount)
      println("withMoveTime = " + withMoveTime)
      println("playAtTime = " + playAtTime)
      println("canonifyTime = " + canonifyTime)
      println("maxIteratorTime = " + maxIteratorTime)
      println("argsMaxTime = " + argsMaxTime)
      println("fTime = " + fTime)
      println("fCount = " + fCount)
      println("fakeTime = " + fakeTime)
      println("hasNextTime = " + hasNextTime)
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
