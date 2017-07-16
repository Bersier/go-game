package board

sealed trait Color {
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

sealed trait ProperColor extends Color {
  override def dual: ProperColor
}

case object Black extends ProperColor {
  override def dual = White
  override def toInt: Int = 1
}

case object White extends ProperColor {
  override def dual = Black
  override def toInt: Int = 2
}
