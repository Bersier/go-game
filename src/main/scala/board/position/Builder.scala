package board.position

import board.{Color, Intersection, None, ProperColor, Size}
import commons.Utils

import scala.collection.mutable

/**
  * Position builder.
  */
private trait Builder {

  /**
    * Sets the color at x.
    */
  def update(x: Intersection, color: Color): Unit

  def build: Position

  final def cleanup(x: Intersection, color: ProperColor) {
    def removeDead(x: Intersection, color: ProperColor)(alive: mutable.Set[Intersection]) {
      val visited = mutable.Set[Intersection]()
      def isAlive(intersection: Intersection): Boolean = {
        if (alive(intersection)) true
        else if (visited(intersection)) false
        else if (apply(intersection) == None) true
        else if (apply(intersection) != color) false
        else {
          visited += intersection
          intersection.neighbors(size).exists(isAlive)
        }
      }

      if (isAlive(x)) {
        alive ++= visited
      } else {
        for (x <- visited) {
          update(x, None)
        }
      }
    }

    val transAlives = mutable.Set[Intersection]()
    for (n <- x.neighbors(size) if apply(n) == color.dual) {
      removeDead(n, color.dual)(transAlives)
    }

    removeDead(x, color)(Utils.MockSet)
  }

  protected[this] def size: Size

  protected[this] def apply(x: Intersection): Color
}
