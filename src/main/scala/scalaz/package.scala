
package categoric

import scala.reflect.internal.annotations.compileTimeOnly

package object `scalaz` {

  def monadic[M[_]] = new ScalazMonadic[M]

  def monadic[M[_], N[_]] = new ScalazMonadic2[M, N]

  def monadic[L[_], M[_], N[_]] = new ScalazMonadic3[L, M, N]

  @compileTimeOnly("`snoop1` must be enclosed in a `monadic` block")
  def snoop1[M[_], T](snoopable: M[T]): T = ???

  @compileTimeOnly("`snoop2` must be enclosed in a `monadic` block")
  def snoop2[M[_], N[_], T](snoopable: M[N[T]]): T = ???

  @compileTimeOnly("`snoop3` must be enclosed in a `monadic` block")
  def snoop3[L[_], M[_], N[_], T](snoopable: L[M[N[T]]]): T = ???

}