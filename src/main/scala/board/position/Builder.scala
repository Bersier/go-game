package board.position

import board.{Color, Intersection, None, ProperColor, Size}
import commons.Utils

import scala.collection.mutable

/**
  * Position builder.
  */
private trait Builder {

  /**
    * Executes play at the given intersection with the given color.
    */
  @inline final def playAt(x: Intersection, color: ProperColor): this.type = {
    update(x, color)
    cleanup(x, color)
    this
  }

  /**
    * @return a Position corresponding to this builder
    */
  def build: Position

  /**
    * @return the color at 'x'
    */
  protected[this] def apply(x: Intersection): Color

  /**
    * Sets the color at x.
    */
  protected[this] def update(x: Intersection, color: Color): Unit

  /**
    * Removes all stones that died from player 'color' playing at 'x'.
    */
  private[this] def cleanup(x: Intersection, color: ProperColor) {
    assert(apply(x) == color)
    def removeDead(x: Intersection, color: ProperColor)(alive: mutable.Set[Intersection]) {
      val visited = mutable.Set[Intersection]()
      def isAlive(intersection: Intersection): Boolean = {
        if (alive(intersection)) true
        else if (visited(intersection)) false
        else if (apply(intersection) == None) true
        else if (apply(intersection) != color) false
        else {
          visited += intersection
          intersection.neighbors.exists(isAlive)
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
    for (n <- x.neighbors if apply(n) == color.dual) {
      removeDead(n, color.dual)(transAlives)
    }

    removeDead(x, color)(Utils.MockSet)
  }
}

private object Builder {

  def apply(stateDescription: (Int, Int) => Color)(implicit size: Size): Builder = {
    new DefaultPosition(stateDescription)
  }
}