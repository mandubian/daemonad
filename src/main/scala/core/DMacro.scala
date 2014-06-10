/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe


abstract class DMacro {

  def blockImpl[M[_], A: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[A])
                                       (implicit mw: c.WeakTypeTag[M[_]]): c.Expr[M[A]]

  def blockImpl2[M[_], N[_], A: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[A])
                                       (implicit mw: c.WeakTypeTag[M[_]],
                                                 nw: c.WeakTypeTag[N[_]]): c.Expr[M[N[A]]]

  def blockImpl3[M[_], N[_], O[_], A: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[A])
                                       (implicit mw: c.WeakTypeTag[M[_]],
                                                 nw: c.WeakTypeTag[N[_]],
                                                 ow: c.WeakTypeTag[O[_]]): c.Expr[M[N[O[A]]]]


  def blockImpl4[M[_], N[_], O[_], P[_], A: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[A])
                                       (implicit mw: c.WeakTypeTag[M[_]],
                                                 nw: c.WeakTypeTag[N[_]],
                                                 ow: c.WeakTypeTag[O[_]],
                                                 pw: c.WeakTypeTag[P[_]]): c.Expr[M[N[O[P[A]]]]]
}