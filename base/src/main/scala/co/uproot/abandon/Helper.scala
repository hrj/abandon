package co.uproot.abandon

object Helper {
  val Zero = BigDecimal(0)
  def maxElseZero(s: Iterable[Int]) = if (s.nonEmpty) s.max else 0

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
  private val shortMonths = List("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")

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

  def getShortMonth(monthNumber:Int) = {
    shortMonths(monthNumber - 1)
  }

}
