
package categoric

import scala.reflect.internal.annotations.compileTimeOnly

package object `scalaz` {

  def monadic[M[_]] = new ScalazMonadic[M]

  def monadic[M[_], N[_]] = new ScalazMonadic2[M, N]

  @compileTimeOnly("`peek` must be enclosed in a `monadic` block")
  def peek[M[_], T](peekable: M[T]): T = ???

  @compileTimeOnly("`peek2` must be enclosed in a `monadic` block")
  def peek2[M[_], N[_], T](peekable: M[N[T]]): T = ???

}