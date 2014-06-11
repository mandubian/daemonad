/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad
package scalaz

import core._
import monad._


trait ScalazMonadUtils extends MonadUtils with DContext {

  import c.universe._
  import c.internal._
  import decorators._

  def monadPoint(mType: TpeHelper)(fa: Tree): Tree = {
    q"""
      implicitly[Monad[${mType.tpe}]].point($fa)
    """
  }

  def monadPoint(impl: Tree)(fa: Tree): Tree = {
    q"""
      $impl.point($fa)
    """
  }

  override def monadMap(mType: TpeHelper)(fa: Tree)(fn: Tree): Tree = {
    q"""
      implicitly[Monad[${mType.tpe}]].map($fa)($fn)
    """
  }

  override def monadMap(impl: Tree)(fa: Tree)(fn: Tree): Tree = {
    q"""
      $impl.map($fa)($fn)
    """
  }

  override def monadBind(mType: TpeHelper)(fa: Tree)(fn: Tree): Tree = {
    q"""
      implicitly[Monad[${mType.tpe}]].bind($fa)($fn)
    """
  }

  override def monadBind(impl: Tree)(fa: Tree)(fn: Tree): Tree = {
    q"""
      $impl.bind($fa)($fn)
    """
  }

  override def monadTMap(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    val tree = q"""
      ${constructor(fa)}.map($fn)
    """

    extractor(tree)
  }

  override def monadTMap(from: Type, to: Type, constructor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    q"""
      ${constructor(fa)}.map($fn)
    """
  }

  override def monadTBind(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    val tree = q"""
      ${constructor(fa)}.flatMap($fn)
    """
    extractor(tree)
  }

  override def monadTBind(from: Type, to: Type, constructor: Tree => Tree)(fa: Tree)(fn: Tree): Tree = {
    val tree = q"""
      ${constructor(fa)}.flatMap($fn)
    """
    tree
  }

}


