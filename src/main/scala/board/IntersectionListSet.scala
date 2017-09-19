package board

import scala.collection.mutable

final class IntersectionListSet private(private var representation: Array[Int])
  extends mutable.Set[Intersection] {

  val i = "".asInstanceOf[Int].asInstanceOf[Intersection]

  override def +=(elem: Intersection) = ???

  override def -=(elem: Intersection) = ???

  override def contains(elem: Intersection) = ???

  override def iterator = ???
}
