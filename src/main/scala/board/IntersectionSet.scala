package board

import scala.collection.mutable

final class IntersectionSet private(private val representation: Array[Int])
  extends mutable.Set[Intersection] {

  override def +=(x: Intersection): this.type = {
    representation(x.i) |= 1 << x.j
    this
  }

  override def -=(x: Intersection): this.type = {
    representation(x.i) &= ~(1 << x.j)
    this
  }

  override def ++=(xs: TraversableOnce[Intersection]): this.type = xs match {
    case xs: IntersectionSet => {
      for (i <- xs.representation.indices) {
        representation(i) |= xs.representation(i)
      }
      this
    }
    case _ => super.++=(xs)
  }

  override def contains(x: Intersection): Boolean = {
    ((representation(x.i) >>> x.j) & 1) == 1
  }

  override def iterator: Iterator[Intersection] = new Iterator[Intersection] {
    private[this] var i = 0
    private[this] var j = 0
    private[this] var row = representation(0)

    hasNext()

    override def hasNext(): Boolean = {
      if (row == 0) {
        i += 1
        if (i == representation.length) false
        else {
          j = 0
          row = representation(i)
          hasNext()
        }
      }
      else {
        while ((row & 1) == 0) {
          j += 1
          row >>>= 1
        }
        true
      }
    }

    override def next(): Intersection = {
      val x = Intersection(i, j)
      j += 1
      row >>>= 1
      x
    }
  }
}

object IntersectionSet {
  def empty(implicit size: Size): IntersectionSet = {
    assert(size > 0)
    assert(size <= 32)
    new IntersectionSet(Array.ofDim(size))
  }
}
