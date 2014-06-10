/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad

import scala.reflect.internal.annotations.compileTimeOnly
import scala.language.experimental.macros


package object `scalaz` {

  def monadic[M[_]] = new ScalazMonadic[M]

  def monadic[M[_], N[_]] = new ScalazMonadic2[M, N]

  def monadic[M[_], N[_], O[_]] = new ScalazMonadic3[M, N, O]

  def monadic[M[_], N[_], O[_], P[_]] = new ScalazMonadic4[M, N, O, P]

  @compileTimeOnly("`snoop1` must be enclosed in a `monadic` block")
  def snoop1[M[_], T](snoopable: M[T]): T = ???

  @compileTimeOnly("`snoop2` must be enclosed in a `monadic` block")
  def snoop2[M[_], N[_], T](snoopable: M[N[T]]): T = ???

  @compileTimeOnly("`snoop3` must be enclosed in a `monadic` block")
  def snoop3[L[_], M[_], N[_], T](snoopable: L[M[N[T]]]): T = ???

  @compileTimeOnly("`snoop4` must be enclosed in a `monadic` block")
  def snoop4[L[_], M[_], N[_], T](snoopable: L[M[N[T]]]): T = ???

}