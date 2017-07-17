package board.position

import board.{Color, Intersection, None, PlayerColor, Size}
import commons.Utils

import scala.collection.mutable

/**
  * Position builder.
  */
private trait Builder {

  /**
    * Executes play at the given intersection with the given color.
    */
  @inline final def playAt(x: Intersection, color: PlayerColor)(implicit size: Size): this.type = {
    this(x) = color
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
    * Updates all the given intersections to the given colors.
    */
  protected[position] def update(updates: TraversableOnce[(Intersection, Color)]): this.type = {
    for ((x, color) <- updates) {
      this(x) = color
    }
    this
  }

  /**
    * Removes all stones that died from player 'color' playing at 'x'.
    */
  private[this] def cleanup(x: Intersection, color: PlayerColor)(implicit size: Size) {
    assert(apply(x) == color)
    def removeDead(x: Intersection, color: PlayerColor)(alive: mutable.Set[Intersection]) {
      val visited = mutable.Set[Intersection]()
      def isAlive(x: Intersection): Boolean = {
        if (alive(x)) true
        else if (visited(x)) false
        else if (apply(x) == None) true
        else if (apply(x) != color) false
        else {
          visited += x
          x.neighbors.exists(isAlive)
        }
      }

      if (isAlive(x)) {
        alive ++= visited
      } else {
        for (x <- visited) {
          this(x) = None
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

  def initial(implicit size: Size): Builder = apply((_, _) => None)

  def apply(stateDescription: (Int, Int) => Color)(implicit size: Size): Builder = {
    new DefaultPosition(stateDescription)
  }
}