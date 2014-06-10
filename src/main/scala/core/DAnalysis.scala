/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core


trait DAnalysis extends TransformUtils {
  import c.universe._

  def reportUnsupportedSnoops(monadTpes: List[TpeHelper], tree: Tree): Unit

}