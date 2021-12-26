package clara.util

import scala.collection.IterableOnce

object Safe {
  implicit class SafeEquals[T](val l: T) extends AnyVal {
    @inline def ===(r: T): Boolean = l == r
    @inline def !==(r: T): Boolean = l != r
  }

  implicit class StringsSafeString(val ss: IterableOnce[String]) extends AnyVal {
    @inline def safeString(prefix: String, sep: String, suffix: String): String =
      ss.iterator.mkString(prefix, sep, suffix)
    @inline def safeString(sep: String): String = ss.iterator.mkString(sep)
    @inline def safeString: String = ss.iterator.mkString
  }

  implicit class IntSafeString(val v: Int) extends AnyVal {
    @inline def safeString: String = v.toString()
  }

  implicit class LongSafeString(val v: Long) extends AnyVal {
    @inline def safeString: String = v.toString()
  }

  implicit class SafeStringContext(val stringContext: StringContext) extends AnyVal {
    def safe(args: String*): String = {
      val process = StringContext.processEscapes _
      val pi = stringContext.parts.iterator
      val ai = args.iterator
      val sb = new java.lang.StringBuilder(process(pi.next()))
      while (ai.hasNext) {
        sb.append(ai.next())
        sb.append(process(pi.next()))
      }
      sb.toString
    }
  }
}
