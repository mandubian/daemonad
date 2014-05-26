package categoric
package core

trait CategoricAnalysis extends TransformUtils {
  import c.universe._

  def reportUnsupportedSnoops(tree: Tree, firstDepth: Int): Unit

}