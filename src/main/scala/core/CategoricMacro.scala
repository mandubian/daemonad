package categoric
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe

abstract class CategoricMacro {

  def blockImpl[M[_], T: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[T])
                                       (implicit mw: c.WeakTypeTag[M[_]]): c.Expr[M[T]]

  def blockImpl2[M[_], N[_], T: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[T])
                                       (implicit mw: c.WeakTypeTag[M[_]],
                                                 nw: c.WeakTypeTag[N[_]]): c.Expr[M[N[T]]]

  def blockImpl3[L[_], M[_], N[_], T: c.WeakTypeTag](c: Context)
                                       (body: c.Expr[T])
                                       (implicit lw: c.WeakTypeTag[L[_]],
                                                 mw: c.WeakTypeTag[M[_]],
                                                 nw: c.WeakTypeTag[N[_]]): c.Expr[L[M[N[T]]]]
}