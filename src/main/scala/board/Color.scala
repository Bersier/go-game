package board

sealed trait Color extends Any {
  def dual: Color
  private[board] def toInt: Int
}

private object Color {

  def fromInt(i: Int): Color = i match {
    case 0 => None
    case 1 => Black
    case 2 => White
  }
}

case object None extends Color {
  override def dual: Color = None
  override def toInt: Int = 0
}

sealed trait PlayerColor extends Any with Color {
  override def dual: PlayerColor
}

case object Black extends PlayerColor {
  override def dual = White
  override def toInt: Int = 1
}

case object White extends PlayerColor {
  override def dual = Black
  override def toInt: Int = 2
}
