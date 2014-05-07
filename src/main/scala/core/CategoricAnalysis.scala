package categoric
package core

trait CategoricAnalysis extends TransformUtils {
  import c.universe._

  def reportUnsupportedPeeks(tree: Tree): Unit

}