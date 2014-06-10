/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe


trait DContext {

  val c: Context

  lazy val splicer = new Splicer[c.type](c)

  import c.universe._
  import c.internal._

  def snoop1MethodSymbol: Symbol
  def snoop2MethodSymbol: Symbol
  def snoop3MethodSymbol: Symbol
  def snoop4MethodSymbol: Symbol

  // def inferMonadTransformer[M[_], N[_], T]
  //       (api: TypingTransformApi)
  //       (implicit mTag: WeakTypeTag[M[_]], nTag: WeakTypeTag[N[_]], tTag: WeakTypeTag[T])
  //       //(mType: Type, nType: Type, resultType: Type)
  //       : (Type, Tree => Tree => Tree, Tree => Tree)

  // def inferMonadTransformerTpe
  //       (api: TypingTransformApi)
  //       (mTpe: Type, nTpe: Type, tTpe: Type)
  //       : (Type, Tree => Tree => Tree, Tree => Tree)

  // def inferMonadTransformerTpe2
  //       (api: TypingTransformApi)
  //       (mTpe: List[Type])
  //       : (Type, Tree => Tree => Tree, Tree => Tree)

}

