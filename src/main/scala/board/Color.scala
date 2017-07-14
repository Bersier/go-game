package board


sealed trait Color {
  def dual: Color
}

case object None extends Color {
  override def dual: Color = None
}

sealed trait ProperColor extends Color {
  override def dual: ProperColor
}

case object Black extends ProperColor {
  override def dual = White
}

case object White extends ProperColor {
  override def dual = Black
}
