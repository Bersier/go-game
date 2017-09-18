package main

import board.{Black, PlayerColor, Size, White}
import board.position.Position
import zobristcode.ZCode128

import scala.collection.mutable

object Main extends App {

  type Positions = mutable.Set[ZCode128]

  for (i <- 1 until 3) {
    println(i + ": " + komi(Size(i)))
  }

  def komi(implicit size: Size): Int = {
    val maxKomi = size * size
    komi(-maxKomi, maxKomi, Black, lastMoveWasPass = false)(Position.initial, mutable.Set.empty[ZCode128], size)
  }

  private
  def komi(min: Int, max: Int, color: PlayerColor)
          (implicit position: Position, prev: Positions, size: Size): Int = withUpdatedPrev {
    val nextPositions = position.nextPositions(color)(prev)
    if (nextPositions.isEmpty) {
      val count = position.count
      val c = count(color) - count(color.dual)
//      println(color + " " + c)
      c
    }
    else {
      def loop(min: Int): Int = {
        val newMin = math.max(min, -komi(-max, -min, color.dual)(nextPositions.next, prev, size))
        if (newMin >= max) newMin
        else if (nextPositions.isEmpty) newMin
        else loop(newMin)
      }
      loop(min)
    }
  }

  private
  def komi(min: Int, max: Int, color: PlayerColor, lastMoveWasPass: Boolean)
          (implicit position: Position, prev: Positions, size: Size): Int = withUpdatedPrev {
    val nextPositions = position.nextPositions(color)(prev)

    def loop(min: Int): Int = {
      if (min >= max || nextPositions.isEmpty) min
      else {
        loop(math.max(
          min,
          -komi(-max, -min, color.dual, lastMoveWasPass = false)(nextPositions.next, prev, size)
        ))
      }
    }

    val newMin = if (lastMoveWasPass) areaDiff(position, color)
    else -komi(-max, -min, color.dual, lastMoveWasPass = true)(position, prev, size)

    loop(newMin)
  }

  private
  def komiBlack(min: Int, max: Int)
               (implicit position: Position, prev: Positions, size: Size): Int = withUpdatedPrev {
    val nextPositions = position.nextPositions(Black)(prev)
    if (nextPositions.isEmpty) countDiff(position)
    else {
      def loop(min: Int): Int = {
        val newMin = math.max(min, komiWhite(min, max)(nextPositions.next, prev, size))
        if (newMin >= max) newMin
        else if (nextPositions.isEmpty) newMin
        else loop(newMin)
      }
      loop(min)
    }
  }

  private
  def komiWhite(min: Int, max: Int)
               (implicit position: Position, prev: Positions, size: Size): Int = withUpdatedPrev {
    val nextPositions = position.nextPositions(White)(prev)
    if (nextPositions.isEmpty) countDiff(position)
    else {
      def loop(max: Int): Int = {
        val newMax = math.min(max, komiBlack(min, max)(nextPositions.next, prev, size))
        if (min >= newMax) newMax
        else if (nextPositions.isEmpty) newMax
        else loop(newMax)
      }
      loop(max)
    }
  }

  private def areaDiff(position: Position, color: PlayerColor) = {
    val area = position.area
    area(color) - area(color.dual)
  }

  private def countDiff(position: Position) = {
    val count = position.count
    println("black! " + (count(Black) - count(White)))
    count(Black) - count(White)
  }

  private var maxPrevCount = 0
  private var callCount = 0

  private
  def withUpdatedPrev[A](getA: => A)(implicit position: Position, prev: Positions): A = {
    if (callCount % 100000 == 0) println("callCount = " + callCount)
    callCount += 1
//    println("<")
//    println(position)
    prev += position.toZobristCode
    if (maxPrevCount < prev.size) {
//      print(position)
      maxPrevCount = prev.size
      println(s"maxPrevCount = $maxPrevCount\n")
    }
    val a = getA
    prev -= position.toZobristCode
//    print(">")
    a
  }
}
