/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But simply copied from Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core


trait PrinterUtils {

  private def enabled(level: String) = sys.props.getOrElse(s"daemonad.$level", "false").equalsIgnoreCase("true")

  private def verbose = enabled("debug")
  private def trace   = enabled("trace")

  def vprintln(s: => Any): Unit = {
    /*if (verbose)*/ println(s"[daemonad] $s")
  }

  def vtrace(s: => Any): Unit = if (trace) println(s"[daemonad] $s")
}
