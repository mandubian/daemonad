package categoric
package monadic

import scala.reflect.macros.Context

import core._

trait MonadicUtils extends CategoricContext with TransformUtils {

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


trait MonadicUpstack extends CategoricContext with MonadicUtils {

  import c.universe._

  /*def type2seq(tpe: Type): Seq[Type] = {
    tpe.dealias.typeArgs match {
      case List()       => Seq(tpe)
      case htpe :: ttpes => htpe +: type2seq(htpe) // doesn't care about ttpes
    }
  }

  def compareTypes(tpe1: Seq[Type], tpe2: Seq[Type]): Boolean = {
    (tpe1, tpe2) match {
      case (h1 :: t1, h2 :: t2) if (h1 =:= h2) => compareTypes(t1, t2)
      case _ => false
    }
  }*/

  def isLastUnit(tpe: Type): Boolean = {
    def step(tpe: Type): Boolean = {
      tpe.dealias.typeArgs match {
        case List()       => tpe =:= definitions.UnitTpe
        // doesn't care about tail types
        case head :: _ => step(head)
      }
    }

    step(tpe)
  }

  def appliedTypes(tpes: Seq[TpeHelper]): TpeHelper =
    tpes match {
      case List()     => throw new RuntimeException("Can't use appliedTypes on empty type list")
      case List(tpeh) => tpeh
      case h :: t     => TpeHelper(appliedType(h.tpe, List(appliedTypes(t).tpe)))
    }

  // M[N[T]] => stack = [M, N, T]
  def upstack(stack: Seq[TpeHelper], tree: Tree, checkLast: Boolean = true): Tree = {
    def step(stack: Seq[TpeHelper], tpe: TpeHelper): Tree = {
      println(s"STACK:$stack tpe:$tpe")

      stack match {
        // empty type stack
        case List() => EmptyTree

        // M
        case List(stpe) =>
          // N[T] <:< M
          if(tpe.tpe =:= stpe.tpe) tree
          else if(tpe.tpe <:< stpe.tpe) {
            if(!checkLast) tree
            else EmptyTree
          }
          else EmptyTree

        case htpe :: ttpe :: Nil =>
          val stackTpe = appliedTypes(stack)

          println(s"1 - TPE:$tpe STACKTPE:$stackTpe htpe:$htpe ttpe:$ttpe ")

          // M[T] =:= M[T]
          if(stackTpe.tpe =:= tpe.tpe) tree
          // T <-> M[T]
          else if(tpe.tpe <:< ttpe.tpe) monadPoint(htpe)(tree)
          // V <-> M[T]
          else {
            if(checkLast) EmptyTree
            else monadPoint(htpe)(tree)
          }

        case htpe :: ttpes =>
          val stackTpe = appliedTypes(stack)

          println(s"2 - TPE:$tpe STACKTPE:$stackTpe htpe:$htpe ttpes:$ttpes")
          // M[N[T]] =:= M[N[T]]
          if(tpe.tpe =:= stackTpe.tpe) tree
          // M[N1[V]] <:< M[N[T]]
          else if(tpe.tpe <:< htpe.tpe) {
            tpe.tpe.dealias.typeArgs match {
              // M[V] <:< M[N[T]]
              case List()  => EmptyTree

              // N1[V] <-> N[T]
              // only takes first parametric type in charge for now
              case subtype :: _ =>
                val subtree = step(ttpes, TpeHelper(subtype))
                if(subtree.isEmpty) EmptyTree
                else monadPoint(htpe)(subtree)
            }
          // M[N[T]] <-> M1[N1[T1]]
          } else {
            tpe.tpe.dealias.typeArgs match {
              // T <-> M[N[T]]
              case List() =>
                val subtree = step(ttpes, tpe)
                if(subtree.isEmpty) EmptyTree
                else monadPoint(htpe)(subtree)

              // P[Q[V]] <-> M[N[T]]
              case _ :: _ =>
                // search in ttpes for `tpe` deeper (even if empty)
                val subtree = step(ttpes, tpe)
                if(subtree.isEmpty) EmptyTree
                else monadPoint(htpe)(subtree)
            }
          }
      }
    }

    val res = step(stack, TpeHelper(tree.tpe))
    println("UPSTACK RES:"+res)
    res
  }
}