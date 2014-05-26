package categoric
package core

trait CategoricTransform extends CategoricContext with TransformUtils {

  import c.universe._
  import c.internal._

  def transform(body: Tree)(monadTpes: List[Type], resultType: Type): Tree

  def inferMonadTransformerTpe3
        (api: TypingTransformApi, mTpe: List[TpeHelper], biTypeMap: collection.mutable.ListBuffer[AliasTpe])
        : AliasTpe
}