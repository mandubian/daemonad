/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core


trait DANF extends DContext {

  import c.universe._

  def anfTransform(tree: Tree): Block

}
