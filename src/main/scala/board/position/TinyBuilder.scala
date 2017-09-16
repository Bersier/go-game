package board.position
import board.{Color, Intersection}
import commons.Utils

import scala.collection.mutable

private class TinyBuilder(reference: PositionInternal[Position]) extends Builder[Position] {

  private[this] val updates = mutable.Map.empty[Intersection, Color]

  override def build: Position = {
    val sizeBitCount = Utils.intLog(reference.size - 1)
    val moveBitCount = 2 * Utils.intLog(reference.size - 1) + 2
    val maxMoveCount = 64 / moveBitCount
    if (updates.size <= maxMoveCount) {
      Position.intersections(reference.size)
        .filterNot(updates.contains)
        .take(maxMoveCount - updates.size)
        .foreach { x => updates(x) = reference(x) }
      var updatesLong = 0L
      for ((b, (x, color)) <- (0 to (64 - moveBitCount) by moveBitCount).zip(updates)) {
        updatesLong |= x.i <<  b
        updatesLong |= x.j << (b + sizeBitCount)
        updatesLong |= color.toInt << (b + 2*sizeBitCount)
      }
      new TinyPosition(reference, updatesLong)
    }
    else new DefaultPosition(reference).update(updates.toIterator).build
  }

  override protected[this] def apply(x: Intersection): Color = updates.getOrElse(x, reference(x))

  override protected[this] def update(x: Intersection, color: Color): Unit = {
    updates(x) = color
  }
}
