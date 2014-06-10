/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad

import scala.reflect.macros.Context

import core._


trait MonadUtils extends DContext with TransformUtils with PrinterUtils {

  import c.universe._

  def monadPoint(mType: TpeHelper)(fa: Tree): Tree

  def monadMap(mType: TpeHelper)(fa: Tree)(fn: Tree): Tree

  def monadBind(mType: TpeHelper)(fa: Tree)(fn: Tree): Tree

  def monadTMap(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree

  def monadTBind(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree

  def monadPoint(impl: Tree)(fa: Tree): Tree

  def monadMap(impl: Tree)(fa: Tree)(fn: Tree): Tree

  def monadBind(impl: Tree)(fa: Tree)(fn: Tree): Tree

  def monadTMap(from: Type, to: Type, constructor: Tree => Tree)(fa: Tree)(fn: Tree): Tree

  def monadTBind(from: Type, to: Type, constructor: Tree => Tree)(fa: Tree)(fn: Tree): Tree
}


trait MonadUpstack extends DContext with MonadUtils {

  import c.universe._

  // M[N[T]] => stack = [M, N, T]
  // None            => use tree as is
  // Some(EmptyTree) => nothing found
  // Some(tree)      => restack tree
  def upstackable(stack: Seq[TpeHelper], tree: Tree): Boolean = {

    def step(stack: Seq[TpeHelper], tpe: TpeHelper): Boolean = {

      stack match {
        // empty type stack
        case List() => false

        // M
        case List(stpe) =>
          // TPE <:< M
          if(tpe <:< stpe.existential) true
          // TPE =!= M
          else false

        // M[T]
        case htpe :: ttpe :: Nil =>
          val stackTpe = appliedTypes(stack)

          vprintln(s"stackTpe1:$stackTpe tpe:$tpe")
          // TPE <:< M[_]
          if(tpe <:< stackTpe.existential) true
          // TPE <:< T
          else if(tpe <:< ttpe.existential) true
          // TPE =!= M[T]
          else false

        // M[N[...]]
        case htpe :: ttpes =>
          val stackTpe = appliedTypes(stack)

          vprintln(s"stackTpe2:$stackTpe tpe:$tpe")
          // TPE =:= M[...]
          if(tpe =:= stackTpe) true
          // TPE <:< M[_]
          else if(tpe <:< htpe.existential) {
            tpe.dealias.typeArgs match {
              // TPE(M) <:< M[_] IMPOSSIBLE
              case List()  => throw new RuntimeException("impossible case")

              // TPE(M[N1[...]]) <:< M[N[...]] -> check N1[...] vs N[...]
              // WARNING : only takes first parametric type in charge for now
              case subtype :: _ =>
                step(ttpes, TpeHelper(subtype))
            }
          // TPE =!= M[...]
          } else {
            step(ttpes, tpe)
          }
      }
    }

    val res = step(stack, TpeHelper(tree.tpe))
    res
  }
  // M[N[T]] => stack = [M, N, T]
  // None            => use tree as is
  // Some(EmptyTree) => nothing found
  // Some(tree)      => restack tree
  def upstack(stack: Seq[TpeHelper], tree: Tree): Option[Tree] = {

    def step(stack: Seq[TpeHelper], tpe: TpeHelper): Option[Tree] = {

      stack match {
        // empty type stack
        case List() => Some(EmptyTree)

        // M
        case List(stpe) =>
          // TPE <:< M
          if(tpe <:< stpe) None
          // TPE =!= M
          else Some(EmptyTree)

        // M[T]
        case htpe :: ttpe :: Nil =>
          val stackTpe = appliedTypes(stack)

          // TPE <:< M[_]
          if(tpe <:< stackTpe.existential) None
          // TPE <:< T
          else if(tpe <:< ttpe) Some(monadPoint(htpe)(tree))
          // TPE =!= M[T]
          else Some(EmptyTree)

        // M[N[...]]
        case htpe :: ttpes =>
          val stackTpe = appliedTypes(stack)

          // TPE =:= M[...]
          if(tpe =:= stackTpe) None
          // TPE <:< M[_]
          else if(tpe <:< htpe.existential) {
            tpe.dealias.typeArgs match {
              // TPE(M) <:< M[_] IMPOSSIBLE
              case List()  => throw new RuntimeException("impossible case")

              // TPE(M[N1[...]]) <:< M[N[...]] -> check N1[...] vs N[...]
              // WARNING : only takes first parametric type in charge for now
              case subtype :: _ =>
                val subtree = step(ttpes, TpeHelper(subtype))
                subtree match {
                  case None => None
                  case Some(s) =>
                    if(s.isEmpty) Some(EmptyTree)
                    else Some(monadPoint(htpe)(s))
                }
            }
          // TPE =!= M[...]
          } else {
            val subtree = step(ttpes, tpe)
            subtree match {
              case None => Some(monadPoint(htpe)(tree))
              case Some(s) =>
                if(s.isEmpty) Some(EmptyTree)
                else Some(monadPoint(htpe)(s))
            }
          }
      }
    }

    val res = step(stack, TpeHelper(tree.tpe))
    res
  }
}