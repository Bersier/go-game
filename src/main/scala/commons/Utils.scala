package commons

import java.security.SecureRandom

import board.Intersection

import scala.collection.mutable
import scala.util.Random

object Utils {

  val secureRandom = new SecureRandom

  object MockSet extends mutable.Set[Intersection] {

    override def +=(elem: Intersection): MockSet.type = this

    override def -=(elem: Intersection): MockSet.type = this

    override def contains(elem: Intersection) = false

    override def iterator = Iterator()

    override def ++=(xs: TraversableOnce[Intersection]): MockSet.type = this

    override def --=(xs: TraversableOnce[Intersection]): MockSet.type = this
  }

  def shuffledRange(n: Short): Array[Short] = {
    val result = (0 until n).view.map(_.toShort).toArray
    shuffle(result)
    result
  }

  def shuffledRange(n: Byte): Array[Byte] = {
    val result = Array.ofDim[Byte](n)
    for (i <- 1 until n) {
      val r = Random.nextInt(i + 1)
      result(i) = result(r)
      result(r) = i.toByte
    }
    result
  }

  def cheapShuffledRange(n: Int): Iterable[Int] = new Iterable[Int] {
    private[this] val source = intPseudoShuffle(intLog(n - 1) + 1)

    override def iterator: Iterator[Int] = new Iterator[Int] {
      private[this] var i = 0
      private[this] var used = 0

      override def hasNext: Boolean = used < n

      override def next(): Int = {
        def findNext(): Int = {
          val candidate: Int = source(i)
          i += 1
          if (candidate < n) candidate
          else findNext()
        }
        used += 1
        findNext()
      }
    }
  }

  def intLog(n: Int): Int = {
    def intLog(n: Int, acc: Int): Int = {
      if (n == 0) acc else intLog(n >>> 1, acc + 1)
    }
    intLog(n, 0)
  }

  /**
    * The permutation of an element is obtained by applying a fixed random permutation of its bits
    * and a random bitmask to it.
    *
    * All possible permutations obtained in this way form a subgroup of the permutation group.
    */
  def intPseudoShuffle(log2: Int) = new IndexedSeq[Int] {
    assert(log2 + 1 < 32)

    private[this] val mask = Random.nextInt >>> (32 - log2)
    private[this] val mixer = shuffledRange(log2.toByte)

    override def length: Int = 1 << log2

    override def apply(n: Int): Int = {
      var result = 0
      for (i <- 0 until log2) {
        result |= ((n >> i) & 1) << mixer(i)
      }
      result ^ mask
    }
  }

  // can be improved by using a binary tree bit set where the nodes keep track of unused bits below
  def shuffledRange(n: Int): TraversableOnce[Int] = new Iterator[Int] {
    private[this] var used = {
      val bitSet: mutable.BitSet = mutable.BitSet(n - 1)
      bitSet -= n - 1
      bitSet
    }
    private[this] var i = n
    private[this] var last: Array[Int] = _

    override def hasNext: Boolean = i > 0

    override def next(): Int = {
      def getUnusedRandomIndex: Int = {
        val r = Random.nextInt(n)
        if (used(r)) getUnusedRandomIndex
        else {
          used += r
          r
        }
      }

      i -= 1
      if (i - 2 < (n >> 5)) {
        if (last == null) {
          last = (0 until n).view.filterNot(used).toArray
          used = null
          shuffle(last)
        }
        last(i)
      }
      else getUnusedRandomIndex
    }
  }

  def shuffle[@specialized(Byte) @specialized(Short) @specialized(Int) A](array: Array[A]) {
    for (i <- array.length - 1 to 0 by -1) {
      val r = Random.nextInt(i + 1)
      val temp = array(i)
      array(i) = array(r)
      array(r) = temp
    }
  }

  def rotate(n: Int , i: Int): Long = (n >>> i) | (n << (32 - i))
  def rotate(n: Long, i: Int): Long = (n >>> i) | (n << (64 - i))


}
