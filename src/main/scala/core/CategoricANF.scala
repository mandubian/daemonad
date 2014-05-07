package categoric
package core

trait CategoricANF extends CategoricContext {

  import c.universe._

  def anfTransform(tree: Tree): Block

}
