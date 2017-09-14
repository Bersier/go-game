package board.position

import board.{Color, Dihedral4, Intersection, None, PlayerColor, Size}
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

  def buildCanonical(implicit size: Size): Position = {
    val colorIterators: IndexedSeq[(Iterator[Int], Dihedral4)] =
      Dihedral4.elements.map(e => (Position.orderedIntersections.map(x => this(e(x)).toInt), e))
    Utils.maxIterator(colorIterators, size*size)
    ???
  }

  /**
    * @return the color at the specified intersection
    */
  protected[this] def apply(i: Int, j: Int): Color = apply(Intersection(i, j))

  /**
    * @return the color 'x'
    */
  protected[this] def apply(x: Intersection): Color = apply(x.i, x.j)

  /**
    * Sets the color at the specified intersection.
    */
  protected[this] def update(i: Int, j: Int, color: Color): Unit = update(Intersection(i, j), color)

  /**
    * Sets the color at 'x'.
    */
  protected[this] def update(x: Intersection, color: Color): Unit = update(x.i, x.j, color)

  /**
    * Updates all the given intersections to the given colors.
    */
  protected[position] def update(updates: TraversableOnce[(Intersection, Color)]): this.type = {
    for ((x, color) <- updates) {
      this(x) = color
    }
    this
  }

  private[this] def rotate(implicit size: Size): Unit = {
    for (i <- 0 until (size + 1) / 2) {
      for (j <- 0 until size / 2) {
        val temp = this(i, j)
        this(i, j) = this(j, size - 1 - i)
        this(j, size - 1 - i) = this(size - 1 - i, size - 1 - j)
        this(size - 1 - i, size - 1 - j) = this(size - 1 - j, i)
        this(size - 1 - j, i) = temp
      }
    }
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