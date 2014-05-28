package co.uproot.abandon

object Helper {
  val Zero = BigDecimal(0)
  def maxElseZero(s: Iterable[Int]) = if (s.nonEmpty) s.max else 0
  def minElseZero(s: Iterable[Int]) = if (s.nonEmpty) s.min else 0
  def sum(s: Iterable[BigDecimal]) = s.foldLeft(Zero)(_ + _)
  def sumDeltas(s: Iterable[DetailedTransaction]) = s.foldLeft(Zero)(_ + _.delta)

  def filterByType[T](s: Seq[_ >: T])(implicit m: Manifest[T]) = s.collect({ case t: T => t })

  def bold(s: String) = Console.BOLD + s + Console.RESET
  def warn(s: String) = Console.RED + s + Console.RESET

  val monthLabels = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  private var counter = 0
  def getUniqueInt = {
    synchronized {
      val prev = counter
      counter += 1
      prev
    }
  }

  private val months = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  private val shortMonths = List("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  private def getIndexOf[T](l: Seq[T], e: T) = {
    val index = l.indexOf(e)
    if (index >= 0) {
      Some(index + 1)
    } else {
      None
    }
  }

  def getMonthNumber(monthStr: String) = {
    getIndexOf(months, monthStr).orElse(getIndexOf(shortMonths, monthStr))
  }

  def getShortMonth(monthNumber: Int) = {
    shortMonths(monthNumber - 1)
  }

}

// The sole purpose of this class is to ensure that equality is checked using references
final class RefWrap[T <: AnyRef](val t: T) {
  override def hashCode() = {
    t.hashCode
  }

  override def equals(that: Any) = {
    that match {
      case thatGroup: RefWrap[_] =>
        t eq thatGroup.t
      case _ => false
    }
  }
}
