package categoric
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe

trait CategoricContext {

  val c: Context

  lazy val splicer = new Splicer[c.type](c)

  import c.universe._
  import c.internal._

  def peekMethodSymbol: Symbol
  def peek2MethodSymbol: Symbol

  def inferMonadTransformer[M[_], N[_], T]
        (api: TypingTransformApi)
        (implicit mTag: WeakTypeTag[M[_]], nTag: WeakTypeTag[N[_]], tTag: WeakTypeTag[T])
        //(mType: Type, nType: Type, resultType: Type)
        : (Type, Tree => Tree => Tree, Tree => Tree)
}

