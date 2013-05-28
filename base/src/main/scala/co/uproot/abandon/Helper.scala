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
}
