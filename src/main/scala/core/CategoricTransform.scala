package categoric
package core

trait CategoricTransform extends CategoricContext {

  import c.universe._

  def transform[M[_], T](body: Tree)(mType: WeakTypeTag[M[_]], resultType: WeakTypeTag[T]): Tree
}