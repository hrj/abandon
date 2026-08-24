package co.uproot.abandon.web

import java.text.DecimalFormat

private[web] object JsonUtils {
  private val decimalFormat = new DecimalFormat("##,##,##,##,##,##,##0.00")

  def serializeJSON(any: Any): Array[Char] = {
    any match {
      case map: Map[?, ?] =>
        val sb = StringBuilder(1024)
        val keys = map.keys.toList
        val maxIndex = keys.size - 1
        sb.append('{')
        keys.zipWithIndex.foreach(ki => {
          val key = ki._1
          val index = ki._2
          val value = map(key)
          sb.append('"')
          sb.append(key)
          sb.append('"')
          sb.append(':')
          sb.appendAll(serializeJSON(value))
          if (index != maxIndex) {
            sb.append(',')
          }
        })
        sb.append('}')
        sb.toCharArray
      case array: Array[Any] =>
        val sb = StringBuilder(1024)
        val maxIndex = array.length - 1
        // test
        sb.append('[')
        array.zipWithIndex.foreach(vi => {
          val v = vi._1
          val index = vi._2
          sb.appendAll(serializeJSON(v))
          if (index != maxIndex) {
            sb.append(',')
          }
        })
        sb.append(']')
        sb.toCharArray
      case b: Boolean => ("" + b).toCharArray
      case bd: BigDecimal => escapeAsJSONString(decimalFormat.format(bd.toDouble)).toCharArray
      case s: String => escapeAsJSONString(s).toCharArray
      case any: Any => escapeAsJSONString(any.toString).toCharArray
    }
  }

  def escapeAsJSONString(string: String): String = {
    if (string == null || string.isEmpty) return "\"\""
    val len = string.length
    val sb = new StringBuilder(len + 4)
    var t: String = null
    sb.append('"')
    string.foreach {
      case '\\' =>
      case c@'"' =>
        sb.append('\\')
        sb.append(c)

      case c@'/' =>
        //                if (b == '<') {
        sb.append('\\')
        //                }
        sb.append(c)

      case '\b' =>
        sb.append("\\b")

      case '\t' =>
        sb.append("\\t")

      case '\n' =>
        sb.append("\\n")

      case '\f' =>
        sb.append("\\f")

      case '\r' =>
        sb.append("\\r")

      case c =>
        if (c < ' ') {
          t = "000" + Integer.toHexString(c)
          sb.append("\\u" + t.substring(t.length - 4))
        }
        else sb.append(c)
    }
    sb.append('"')
    sb.toString
  }
}
