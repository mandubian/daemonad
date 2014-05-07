package categoric
package core

object PrinterUtils {

  private def enabled(level: String) = sys.props.getOrElse(s"categoric.$level", "false").equalsIgnoreCase("true")

  private def verbose = enabled("debug")
  private def trace   = enabled("trace")

  private[categoric] def vprintln(s: => Any): Unit = if (verbose) println(s"[categoric] $s")

  private[categoric] def trace(s: => Any): Unit = if (trace) println(s"[categoric] $s")
}
