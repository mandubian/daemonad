/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad
package scalaz

import scala.language.experimental.macros

import core._
import monad._
import scala.reflect.internal.annotations.compileTimeOnly


class ScalazMonadic[M[_]] {

  def apply[A](body: A): M[A] = macro ScalazMonadMacro.blockImpl[M, A]

}

class ScalazMonadic2[M[_], N[_]] {

  def apply[A](body: A): M[N[A]] = macro ScalazMonadMacro.blockImpl2[M, N, A]

}

class ScalazMonadic3[M[_], N[_], O[_]] {

  def apply[A](body: A): M[N[O[A]]] = macro ScalazMonadMacro.blockImpl3[M, N, O, A]

}


class ScalazMonadic4[M[_], N[_], O[_], P[_]] {

  def apply[A](body: A): M[N[O[P[A]]]] = macro ScalazMonadMacro.blockImpl4[M, N, O, P, A]

}


