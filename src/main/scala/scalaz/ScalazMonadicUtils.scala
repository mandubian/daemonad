package categoric
package scalaz

import core._
import categoric.monadic._

trait ScalazMonadicUtils extends MonadicUtils {
  self: CategoricContext =>

  import c.universe._
  import c.internal._
  import decorators._

  def monadPoint(mType: Type)(fa: Tree): Tree = {
    q"""
      implicitly[Monad[$mType]].point($fa)
    """
  }

  override def monadMap(mType: Type)(fa: Tree)(fn: Tree): Tree = {
    q"""
      implicitly[Monad[$mType]].map($fa)($fn)
    """
  }

  override def monadBind(mType: Type)(fa: Tree)(fn: Tree): Tree = {
    q"""
      implicitly[Monad[$mType]].bind($fa)($fn)
    """
  }

  override def monadTMap(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    //val TypeDef(_, sym, _, _) = mType
    val tree = q"""
      implicitly[$mType].map(${constructor(fa)})($fn)
    """

    extractor(tree)
  }

  override def monadTBind(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    //val TypeDef(_, sym, _, _) = mType
    val tree = q"""
      implicitly[$mType].bind(${constructor(fa)})($fn)
    """

    extractor(tree)
  }

}


