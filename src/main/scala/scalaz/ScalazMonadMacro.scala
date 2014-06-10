/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad
package scalaz

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.reflect.internal.Flags

import core._
import monad._


object ScalazMonadMacro extends DMacro with PrinterUtils {

  override def blockImpl[M[_], T: c0.WeakTypeTag]
                (c0: Context)
                (body: c0.Expr[T])
                (implicit mw: c0.WeakTypeTag[M[_]]): c0.Expr[M[T]] = {

    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadTransform {
      val c: c0.type = c0
    }

    //val code = system.transform1[M, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[T])
    val code = transformer.transform(body.tree)(
      weakTypeTag[M[_]].tpe :: Nil, weakTypeTag[T].tpe
    )

    vprintln("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[T]](code)
  }

  override def blockImpl2[M[_], N[_], T: c0.WeakTypeTag]
                (c0: Context)
                (body: c0.Expr[T])
                (implicit mw: c0.WeakTypeTag[M[_]],
                          nw: c0.WeakTypeTag[N[_]]): c0.Expr[M[N[T]]] = {

    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadTransform {
      val c: c0.type = c0
    }

    //val code = transformer.transform2[M, N, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[N[_]], weakTypeTag[T])
    val code = transformer.transform(body.tree)(
      weakTypeTag[M[_]].tpe :: weakTypeTag[N[_]].tpe :: Nil, weakTypeTag[T].tpe
    )

    vprintln("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[N[T]]](code)
  }

  override def blockImpl3[M[_], N[_], O[_], T: c0.WeakTypeTag]
                (c0: Context)
                (body: c0.Expr[T])
                (implicit mw: c0.WeakTypeTag[M[_]],
                          nw: c0.WeakTypeTag[N[_]],
                          ow: c0.WeakTypeTag[O[_]]): c0.Expr[M[N[O[T]]]] =
  {
    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadTransform {
      val c: c0.type = c0
    }

    //val code = transformer.transform3(body.tree)(weakTypeTag[L[_]].tpe, weakTypeTag[M[_]].tpe, weakTypeTag[N[_]].tpe, weakTypeTag[T].tpe)
    val code = transformer.transform(body.tree)(
      weakTypeTag[M[_]].tpe :: weakTypeTag[N[_]].tpe :: weakTypeTag[O[_]].tpe :: Nil, weakTypeTag[T].tpe)

    vprintln("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[N[O[T]]]](code)
  }


  override def blockImpl4[M[_], N[_], O[_], P[_], T: c0.WeakTypeTag]
                (c0: Context)
                (body: c0.Expr[T])
                (implicit mw: c0.WeakTypeTag[M[_]],
                          nw: c0.WeakTypeTag[N[_]],
                          ow: c0.WeakTypeTag[O[_]],
                          pw: c0.WeakTypeTag[P[_]]): c0.Expr[M[N[O[P[T]]]]] = {

    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadTransform {
      val c: c0.type = c0
    }

    val code = transformer.transform(body.tree)(
      weakTypeTag[M[_]].tpe :: weakTypeTag[N[_]].tpe :: weakTypeTag[O[_]].tpe :: weakTypeTag[P[_]].tpe :: Nil, weakTypeTag[T].tpe)

    vprintln("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[N[O[P[T]]]]](code)
  }

}
