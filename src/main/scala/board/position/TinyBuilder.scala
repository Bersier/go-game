package board.position
import board.{Color, Intersection, Size}
import commons.Utils

import scala.collection.mutable

private class TinyBuilder(reference: Position) extends Builder {

  private[this] val updates = mutable.Map.empty[Intersection, Color]

  override def build: Position = {
    if (updates.size * (2*Utils.intLog(reference.size - 1) + 2) <= 64) {
      //reference.in
    }
    ???
  }

  override protected[this] def apply(x: Intersection): Color = updates.getOrElse(x, reference(x))

  override protected[this] def update(x: Intersection, color: Color): Unit = {
    updates(x) = color
  }
}
