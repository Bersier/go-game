package main

import board.{Black, Size, White}
import board.position.Position

import scala.collection.mutable

object Main extends App {

  type Positions = mutable.Set[Position]

  for (i <- 1 until 20) {
    println(i + ": " + komi(Size(i)))
  }

  def komi(implicit size: Size): Int = {
    komiBlack(Int.MinValue, Int.MaxValue)(Position.initial, mutable.Set.empty[Position], size)
  }

  private
  def komiBlack(min: Int, max: Int)
               (implicit position: Position, prev: Positions, size: Size): Int = withUpdatedPrev {
    val nextPositions = position.nextPositions(Black)(prev.toSet)
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
    val nextPositions = position.nextPositions(White)(prev.toSet)
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

  private def countDiff(position: Position) = {
    val count = position.count
    count(Black) - count(White)
  }

  private var maxPrevCount = 0

  private
  def withUpdatedPrev[A](getA: => A)(implicit position: Position, prev: Positions): A = {
    prev += position
    if (maxPrevCount < prev.size) {
//      print(position)
      maxPrevCount = prev.size
      println(s"maxPrevCount = $maxPrevCount\n")
    }
    val a = getA
    prev -= position
    a
  }
}
