package categoric
package monadic

import scala.reflect.macros.Context

import core._

trait MonadicUtils extends CategoricContext {

  import c.universe._

  def monadPoint(mType: Type)(fa: Tree): Tree

  def monadMap(mType: Type)(fa: Tree)(fn: Tree): Tree

  def monadBind(mType: Type)(fa: Tree)(fn: Tree): Tree

  def monadTMap(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree

  def monadTBind(mType: Type, from: Type, to: Type, constructor: Tree => Tree, extractor: Tree => Tree)(fa: Tree)(fn: Tree): Tree

}


trait MonadicRestack extends CategoricContext with MonadicUtils {

  import c.universe._

  def type2Seq(tpe: Type): Seq[Type] = {
    tpe.dealias.typeArgs match {
      case List()       => Seq(tpe)
      case head :: tail => Seq(tpe) ++ type2seq(t) // doesn't care about tail
    }
  }

  def compareTpes(tpe1: Seq[Type], tpe2: Seq[Type]) = {
    (tpe1, tpe2) match {
      case (h1 :: t1, h2 :: t2) if (h1 =:= h2) => compareTypes(t1, t2)
      case _ => false
    }
  }

  def appliedTypes(tpes: Seq[Type]): Type =
    tpes match {
      case List()    => throw new RuntimeException("Can't appliedTypes on empty type list")
      case List(tpe) => tpe
      case h :: t    => appliedType(h, List(appliedTypes(t)))
    }

  def restack(stack: Seq[Type], tree: Tree): Tree = {
    val tpes = type2Seq(tree.tpe)

    def step(stack: Seq[Type], tpe: Type): Tree = {
      val stackTpe = appliedTypes(stack)

      // TODO pattern match on stack
      if(stack.isEmpty) EmptyTree
      else if(stackTpe =:= tpe) tree
      else if(stackTpe <:< tpe) {
        // find the type from where we must M.point
        val subtype = tpe.dealias.typeArgs(0)
        val subtree = step(stack.tail, subtype)
        if(subtree.isEmpty) EmptyTree
        else monadPoint(stack.head)(subtree)
        // TODO
      } else {
        // try with tail
        val subtree = step(stack.tail, tpe)
        if(subtree.isEmpty) EmptyTree
        else monadPoint(stack.head)(subtree)
      }
    }

    step(stack, tree.tpe)
  }
}