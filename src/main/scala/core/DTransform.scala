/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core


trait DTransform extends DContext with TransformUtils {

  import c.universe._
  import c.internal._

  def transform(body: Tree)(monadTpes: List[Type], resultType: Type): Tree

  def inferMonadTransformerTpe
        (api: TypingTransformApi, knownAliasTpes: collection.mutable.ListBuffer[AliasTpe])(mTpe: List[TpeHelper], resultTpe: TpeHelper)
        : AliasTpe
}