package board

final class Size private(private val value: Int) extends AnyVal

object Size {

  def apply(value: Int) = new Size(value)

  implicit def SizeToInt(size: Size): Int = size.value
}