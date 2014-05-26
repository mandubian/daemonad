package categoric
package scalaz

import scala.language.experimental.macros

import core._
import categoric.monadic._

class ScalazMonadic[M[_]] {

  def apply[T](body: T): M[T] = macro ScalazMonadMacro.blockImpl[M, T]

}

class ScalazMonadic2[M[_], N[_]] {

  def apply[T](body: T): M[N[T]] = macro ScalazMonadMacro.blockImpl2[M, N, T]

}

class ScalazMonadic3[L[_], M[_], N[_]] {

  def apply[T](body: T): L[M[N[T]]] = macro ScalazMonadMacro.blockImpl3[L, M, N, T]

}
