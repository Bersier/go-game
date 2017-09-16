package board.position

import board.{Color, Dihedral4, HorizontalFlip, Identity, Intersection, None, OppositeFlip, PlayerColor, Rotation1, Rotation2, Rotation3, Size, TransposeFlip, VerticalFlip}
import commons.Utils

import scala.collection.mutable

/**
  * Position builder.
  */
private trait Builder {

  /**
    * Executes play at the given intersection with the given color. The current color at 'x' is
    * overwritten.
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
    * @return a canonical Position corresponding to this builder
    */
  def buildCanonical(implicit size: Size): Position = {
    canonify
    build
  }

  def canonify(implicit size: Size): Unit = {
    val colorIterators: IndexedSeq[(Iterator[Int], Dihedral4)] =
      Dihedral4.elements.map(e => (Position.orderedIntersections.map(x => this(e(x)).toInt), e))
    rotoflect(Utils.maxIterator(colorIterators, size * size))
  }

  /**
    * @return the color 'x'
    */
  protected def apply(x: Intersection): Color

  /**
    * Sets the color at 'x'.
    */
  protected def update(x: Intersection, color: Color): Unit

  /**
    * Updates all the given intersections to the given colors.
    */
  protected[position] def update(updates: TraversableOnce[(Intersection, Color)]): this.type = {
    for ((x, color) <- updates) {
      this(x) = color
    }
    this
  }

  protected[this] def rotoflect(element: Dihedral4)(implicit size: Size): Unit = element match {
    case Identity => ()
    case Rotation1 => rotate1
    case Rotation2 => rotate2
    case Rotation3 => rotate3
    case VerticalFlip => verticalFlip
    case HorizontalFlip => horizontalFlip
    case TransposeFlip => transposeFlip
    case OppositeFlip => oppositeFlip
  }

  protected[this] def rotate1(implicit size: Size): Unit = {
    for (i <- 0 until (1 + size) / 2) {
      for (j <- 0 until size / 2) {
        val x0 = Intersection(i, j)
        val x1 = Rotation1(x0)
        val x2 = Rotation2(x0)
        val x3 = Rotation3(x0)

        val temp = this(x0)
        this(x0) = this(x1)
        this(x1) = this(x2)
        this(x2) = this(x3)
        this(x3) = temp
      }
    }
  }

  protected[this] def rotate2(implicit size: Size): Unit = {
    for (index <- 0 until size * size / 2) {
      swap(Intersection.fromIndex(index), Rotation2(Intersection.fromIndex(index)))
    }
  }

  protected[this] def rotate3(implicit size: Size): Unit = {
    for (i <- 0 until (1 + size) / 2) {
      for (j <- 0 until size / 2) {
        val x0 = Intersection(i, j)
        val x1 = Rotation1(x0)
        val x2 = Rotation2(x0)
        val x3 = Rotation3(x0)

        val temp = this(x0)
        this(x0) = this(x3)
        this(x3) = this(x2)
        this(x2) = this(x1)
        this(x1) = temp
      }
    }
  }

  protected[this] def verticalFlip(implicit size: Size): Unit = {
    for (i <- 0 until size / 2) {
      for (j <- 0 until size) {
        swap(Intersection(i, j), VerticalFlip(Intersection(i, j)))
      }
    }
  }

  protected[this] def horizontalFlip(implicit size: Size): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size / 2) {
        swap(Intersection(i, j), HorizontalFlip(Intersection(i, j)))
      }
    }
  }

  protected[this] def transposeFlip(implicit size: Size): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until i) {
        swap(Intersection(i, j), TransposeFlip(Intersection(i, j)))
      }
    }
  }

  protected[this] def oppositeFlip(implicit size: Size): Unit = {
    for (i <- 0 until size) {
      for (j <- 0 until size - 1 - i) {
        swap(Intersection(i, j), OppositeFlip(Intersection(i, j)))
      }
    }
  }

  private[this] def swap(x1: Intersection, x2: Intersection): Unit = {
    val temp = this(x1)
    this(x1) = this(x2)
    this(x2) = temp
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

    removeDead(x, color)(Utils.mockSet)
  }
}

private object Builder {

  def initial(implicit size: Size): Builder = apply((_, _) => None)

  def apply(stateDescription: (Int, Int) => Color)(implicit size: Size): Builder = {
    new DefaultPosition(stateDescription)
  }
}