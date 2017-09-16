package board.position

import board.{Color, Intersection}

trait AbstractPosition extends ((Int, Int) => Color) {

  /**
    * @return the color at the specified intersection
    */
  def apply(i: Int, j: Int): Color = apply(Intersection(i, j))

  /**
    * @return the color at the given intersection
    */
  def apply(x: Intersection): Color = apply(x.i, x.j)
}
