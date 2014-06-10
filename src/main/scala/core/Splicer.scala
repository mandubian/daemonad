/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import scala.language.experimental.macros


/**
  * Splicer macro that re-splice the tree in the enclosing context
  *
  * Proposed by Jason Zaugg <https://gist.github.com/retronym/10640845>
  */
class Splicer[C <: reflect.macros.Context with Singleton](val c: C) {
  /** Safely splice the tree `t` into the enclosing lexical context in which it will
  * be type checked. Rather than directly include `t` in the result, a layer of
  * indirection is used: a call to the macro `changeOwner`.
  *
  * This macro is provied with the symbol of the current enclosing context
  * (`c.enclosingOwner`), and the tree `t`.
  *
  * When it is typechecked, it will have access to another macro context with
  * a new enclosing owner. This is substituted from the old owner.
  *
  * This avoids the tedium of manually creating symbols for synthetic enclosing
  * owners when splicing macro arguments. And it avoids the bugs that still plaugue
  * `untypecheck` (e.g. https://issues.scala-lang.org/browse/SI-8500)
  *
  * This approach only works in cases when you are splicing the arguments into leaf
  * position in the synthetic tree. If you need to splice typed trees *above* untyped
  * trees, it will fail because the typecheck stops descending when it finds a typed
  * tree.
  */
  def apply(t: c.Tree): c.Tree = {
    import c.universe._
    // smuggle the symbol of the current enclosing owner through to the
    // `changeOwner` as the symbol of the tree of its first argument.
    // Tree attachments would be a more principled approach, but they aren't
    // part of the public API.
    val ownerIdent = c.internal.setType(Ident(c.internal.enclosingOwner), typeOf[Any])
    q"_root_.daemonad.core.Splicer.changeOwner($ownerIdent, $t)"
  }

  def apply(owner: c.Symbol, t: c.Tree): c.Tree = {
    import c.universe._
    // smuggle the symbol of the current enclosing owner through to the
    // `changeOwner` as the symbol of the tree of its first argument.
    // Tree attachments would be a more principled approach, but they aren't
    // part of the public API.
    val ownerIdent = c.internal.setType(Ident(owner), typeOf[Any])
    q"_root_.daemonad.core.Splicer.changeOwner($ownerIdent, $t)"
  }
}
 
object Splicer {
 
  def changeOwnerMacro[A](c: Context)(ownerIdent: c.Tree, a: c.Tree): c.Tree = {
    import c.universe._
    val origOwner = ownerIdent.symbol
    val result = c.internal.changeOwner(a, origOwner, c.internal.enclosingOwner)
    result
  }
   
  def changeOwner[A](ownerIdent: Any, a: A): A = macro changeOwnerMacro[A]
}
