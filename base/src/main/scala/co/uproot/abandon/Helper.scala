package co.uproot.abandon

object Helper {
  val Zero = BigDecimal(0)
  def maxElseZero(s: Iterable[Int]) = if (s.nonEmpty) s.max else 0
  def sum(s: Iterable[BigDecimal]) = s.foldLeft(Zero)(_ + _)
  def sumDeltas(s: Iterable[DetailedPost]) = s.foldLeft(Zero)(_ + _.delta)

  def filterByType[T](s: Seq[_ >: T])(implicit classTag: scala.reflect.ClassTag[T]) = s.collect({ case t: T => t })

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

  private val months = List("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  private val shortMonths = List("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

  private def getIndexOf[T](l: Seq[T], e: T) = {
    val index = l.indexOf(e)
    if (index >= 0) {
      Some(index + 1)
    } else {
      None
    }
  }

  def getMonthNumber(monthStrIn: String) = {
    val monthStr = monthStrIn.toLowerCase
    getIndexOf(months, monthStr).orElse(getIndexOf(shortMonths, monthStr))
  }

  def getShortMonth(monthNumber: Int) = {
    shortMonths(monthNumber - 1)
  }

  // Thanks to http://stackoverflow.com/a/23761045
  // This function checks whether all elements are unique. If not it returns the first one that is not unique.
  def allUnique[A](to: IterableOnce[A]): Option[A] = {
    val set = scala.collection.mutable.Set[A]()
    to.iterator.foreach { x =>
      if (set(x)) {
        return Some(x)
      } else {
        set += x
      }
    }
    None
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
