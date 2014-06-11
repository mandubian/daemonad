/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe


trait TransformUtils extends DContext {

  import c.universe._
  import c.internal._
  import decorators._
  import Flag._

  import scala.collection.immutable.ListMap

  def isSnoopX(fun: Tree): Boolean =
       fun.symbol == snoop1MethodSymbol || fun.symbol == snoop2MethodSymbol || fun.symbol == snoop3MethodSymbol || fun.symbol == snoop4MethodSymbol

  def isSnoop1(fun: Tree): Boolean = fun.symbol == snoop1MethodSymbol
  def isSnoop2(fun: Tree): Boolean = fun.symbol == snoop2MethodSymbol
  def isSnoop3(fun: Tree): Boolean = fun.symbol == snoop3MethodSymbol
  def isSnoop4(fun: Tree): Boolean = fun.symbol == snoop4MethodSymbol

  def snoopDepth(fun: Tree): Int = {
    if(isSnoop1(fun)) 1
    else if(isSnoop2(fun)) 2
    else if(isSnoop3(fun)) 3
    else if(isSnoop4(fun)) 4
    else -1
  }

  def monadStackFromSnoopX(fun: Tree, monadTpeHelpers: List[TpeHelper]): List[TpeHelper] = {
    monadTpeHelpers.takeRight(snoopDepth(fun))
  }

  case class TpeHelper(val tpe: Type) {
    lazy val dealias = tpe.dealias
    lazy val etaExpand = tpe.typeConstructor.etaExpand
    lazy val PolyType(params, raw) = etaExpand
    lazy val existential = existentialAbstraction(params, raw)
    def applied(paramTpe: Type) = appliedType(raw, List(paramTpe))
    def applied(paramTpe: TpeHelper) = appliedType(raw, List(paramTpe.tpe))
    lazy val unit = applied(definitions.UnitTpe)

    def =:=(theTpe: Type): Boolean = tpe =:= theTpe
    def =:=(theTpe: TpeHelper): Boolean = tpe =:= theTpe.tpe

    def <:<(theTpe: Type): Boolean = tpe <:< theTpe
    def <:<(theTpe: TpeHelper): Boolean = tpe <:< theTpe.tpe

    def asTree: Tree = tq"$tpe"
    def asType: Type = tpe

    def allTypeArgs: Seq[TpeHelper] = {
      def step(tpe: TpeHelper): Seq[TpeHelper] = {
        tpe.tpe.typeArgs.map(TpeHelper(_)) ++ tpe.tpe.typeArgs.flatMap( t => step(TpeHelper(t)))
      }

      step(this)
    }

    override def toString = tpe.toString
  }


  case class AliasTpe(
    typeName: TypeName,
    companionName: TermName,
    trees: Seq[Tree],
    constructor: Tree => Tree => Tree,
    extractor: Tree => Tree,
    refTpe: TpeHelper,
    params: List[TpeHelper],
    monadParam: TpeHelper
  ) {
    def =:=(tpe: Type): Boolean = refTpe.tpe =:= tpe

    def =:=(tpe: TpeHelper): Boolean = refTpe.tpe =:= tpe.tpe

    def sameParams(atpe: AliasTpe): Boolean =
      (params zip atpe.params).foldLeft(true)( (acc, t) => acc && (t._1 =:= t._2) )
  }

  def blockToList(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case t                  => t :: Nil
  }

  def listToBlock(trees: List[Tree]): Block = trees match {
    case trees @ (init :+ last) =>
      val pos = trees.map(_.pos).reduceLeft(_ union _)
      Block(init, last).setType(last.tpe).setPos(pos)
  }

  def defineVar(api: TypingTransformApi)(prefix: String, tp: Type, pos: Position): ValDef = {
    val sym = api.currentOwner.newTermSymbol(name.fresh(prefix), pos, MUTABLE | SYNTHETIC).setInfo(uncheckedBounds(tp))
    valDef(sym, gen.mkZero(uncheckedBounds(tp))).setType(NoType).setPos(pos)
  }

  def defineVal(api: TypingTransformApi)(prefix: String, lhs: Tree, pos: Position): ValDef = {
    val tpe = lhs.tpe match {
      // eliminate constant type because not good as an Info
      case ct: ConstantType => ct.typeSymbol.asType.toType
      case t => t
    }
    val sym = api.currentOwner.newTermSymbol(name.fresh(prefix), pos, SYNTHETIC).setInfo(uncheckedBounds(tpe))
    //lhs.changeOwner(api.currentOwner, sym)
    valDef(sym, lhs.changeOwner(api.currentOwner, sym)).setType(NoType).setPos(pos)
  }

  def defineValWithType(api: TypingTransformApi)(prefix: String, lhs: Tree, pos: Position, tpe: Type): ValDef = {
    val sym = api.currentOwner.newTermSymbol(name.fresh(prefix), pos, SYNTHETIC).setInfo(tpe)
    //lhs.changeOwner(api.currentOwner, sym)
    valDef(sym, lhs.changeOwner(api.currentOwner, sym)).setType(tpe).setPos(pos)
  }

  def defineParam(api: TypingTransformApi)(prefix: String, pos: Position, tpe: Type): ValDef = {
    val sym = api.currentOwner.newTermSymbol(c.freshName(prefix), pos, PARAM).setInfo(tpe)
    valDef(sym).setType(tpe).setPos(pos)
  }

  def copyVal(api: TypingTransformApi)(tree: Tree, mods: Modifiers, name: Name, tpt: Type, rhs: Tree): ValDef = {
    treeCopy.ValDef(tree, mods, name, tq"$tpt", rhs)
  }

  def copyValChangeTpe(api: TypingTransformApi)(tree: Tree, mods: Modifiers, name: Name, tpt: Type, rhs: Tree): ValDef = {
    val tpe = rhs.tpe match {
      // eliminate constant type because not good as an Info
      case ct: ConstantType => ct.typeSymbol.asType.toType
      case t => t
    }
    val res = treeCopy.ValDef(tree, mods, name, tq"$tpe", rhs) //.setType(tpt)
    res.setSymbol(tree.symbol.setInfo(tpe))
    res
  }

  def forceType(api: TypingTransformApi)(tree: Tree, tpe: Type): Tree = {
    tree.setType(tpe match {
      // eliminate constant type because not good as an Info
      case ct: ConstantType => ct.typeSymbol.asType.toType
      case t => t
    })
  }

  object name {
    val Snoop          = "Snoop"
    val matchRes      = "matchres"
    val ifRes         = "ifres"
    val await         = "await"
    val bindSuffix    = "$bind"

    def fresh(name: TermName): TermName = c.freshName(name)
    def fresh(name: String): String = c.freshName(name)
  }

  // Attributed version of `TreeGen#mkCastPreservingAnnotations`
  def mkAttributedCastPreservingAnnotations(tree: Tree, tp: Type): Tree = {
    atPos(tree.pos) {
      val casted = c.typecheck(gen.mkCast(tree, uncheckedBounds(withoutAnnotations(tp)).dealias))
      Typed(casted, TypeTree(tp)).setType(tp)
    }
  }

  private lazy val Boolean_ShortCircuits: Set[Symbol] = {
    import definitions.BooleanClass
    def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(newTermName(name).encodedName)
    val Boolean_&& = BooleanTermMember("&&")
    val Boolean_|| = BooleanTermMember("||")
    Set(Boolean_&&, Boolean_||)
  }

  private def isByName(fun: Tree): ((Int, Int) => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) (i, j) => true
    else {
      val paramss = fun.tpe.paramss
      val byNamess = paramss.map(_.map(_.asTerm.isByNameParam))
      (i, j) => util.Try(byNamess(i)(j)).getOrElse(false)
    }
  }
  private def argName(fun: Tree): ((Int, Int) => String) = {
    val paramss = fun.tpe.paramss
    val namess = paramss.map(_.map(_.name.toString))
    (i, j) => util.Try(namess(i)(j)).getOrElse(s"arg_${i}_${j}")
  }

  /** Map a list of arguments to:
    * - A list of argument Trees
    * - A list of auxillary results.
    *
    * The function unwraps and rewraps the `arg :_*` construct.
    *
    * @param args The original argument trees
    * @param f  A function from argument (with '_*' unwrapped) and argument index to argument.
    * @tparam A The type of the auxillary result
    */
  private def mapArguments[A](args: List[Tree])(f: (Tree, Int) => (A, Tree)): (List[A], List[Tree]) = {
    args match {
      case args :+ Typed(tree, Ident(tpnme.WILDCARD_STAR)) =>
        val (a, argExprs :+ lastArgExpr) = (args :+ tree).zipWithIndex.map(f.tupled).unzip
        val exprs = argExprs :+ atPos(lastArgExpr.pos.makeTransparent)(Typed(lastArgExpr, Ident(tpnme.WILDCARD_STAR)))
        (a, exprs)
      case args                                            =>
        args.zipWithIndex.map(f.tupled).unzip
    }
  }

  case class Arg(expr: Tree, isByName: Boolean, argName: String)

  /**
   * Transform a list of argument lists, producing the transformed lists, and lists of auxillary
   * results.
   *
   * The function `f` need not concern itself with varargs arguments e.g (`xs : _*`). It will
   * receive `xs`, and it's result will be re-wrapped as `f(xs) : _*`.
   *
   * @param fun   The function being applied
   * @param argss The argument lists
   * @return      (auxillary results, mapped argument trees)
   */
  def mapArgumentss[A](fun: Tree, argss: List[List[Tree]])(f: Arg => (A, Tree)): (List[List[A]], List[List[Tree]]) = {
    val isByNamess: (Int, Int) => Boolean = isByName(fun)
    val argNamess: (Int, Int) => String = argName(fun)
    argss.zipWithIndex.map { case (args, i) =>
      mapArguments[A](args) {
        (tree, j) => f(Arg(tree, isByNamess(i, j), argNamess(i, j)))
      }
    }.unzip
  }

  /** Descends into the regions of the tree that are subject to the
    * translation to a state machine by `async`. When a nested template,
    * function, or by-name argument is encountered, the descent stops,
    * and `nestedClass` etc are invoked.
    */
  trait DaemonadTraverser extends Traverser {
    def nestedClass(classDef: ClassDef) {
    }

    def nestedModule(module: ModuleDef) {
    }

    def nestedMethod(defdef: DefDef) {
    }

    def byNameArgument(arg: Tree) {
    }

    def function(function: Function) {
    }

    def patMatFunction(tree: Match) {
    }

    override def traverse(tree: Tree) {
      tree match {
        case cd: ClassDef          => nestedClass(cd)
        case md: ModuleDef         => nestedModule(md)
        case dd: DefDef            => nestedMethod(dd)
        case fun: Function         => function(fun)
        case m@Match(EmptyTree, _) => patMatFunction(m) // Pattern matching anonymous function under -Xoldpatmat of after `restorePatternMatchingFunctions`
        case q"$fun[..$targs](...$argss)" if argss.nonEmpty =>
          traverse(fun)
          val isInByName = isByName(fun)
          for ((args, i) <- argss.zipWithIndex) {
            for ((arg, j) <- args.zipWithIndex) {
              if (!isInByName(i, j)) traverse(arg)
              else byNameArgument(arg)
            }
          }
        case _                     => super.traverse(tree)
      }
    }
  }


  def withAnnotation(tp: Type, ann: Annotation): Type = withAnnotations(tp, List(ann))

  def withAnnotations(tp: Type, anns: List[Annotation]): Type = tp match {
    case AnnotatedType(existingAnns, underlying) => annotatedType(anns ::: existingAnns, underlying)
    case ExistentialType(quants, underlying) => existentialType(quants, withAnnotations(underlying, anns))
    case _ => annotatedType(anns, tp)
  }

  def withoutAnnotations(tp: Type): Type = tp match {
    case AnnotatedType(anns, underlying) => withoutAnnotations(underlying)
    case ExistentialType(quants, underlying) => existentialType(quants, withoutAnnotations(underlying))
    case _ => tp
  }

  def tpe(sym: Symbol): Type = {
    if (sym.isType) sym.asType.toType
    else sym.info
  }

  def thisType(sym: Symbol): Type = {
    if (sym.isClass) sym.asClass.thisPrefix
    else NoPrefix
  }

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

  def allAppliedTypes(tpes: Seq[TpeHelper]): Seq[TpeHelper] =
    tpes match {
      case List()     => throw new RuntimeException("Can't use allAppliedTypes on empty type list")
      case List(tpeh) => Seq(tpeh)
      case h :: t     =>
        val tail = allAppliedTypes(t)
        TpeHelper(appliedType(h.tpe, List(tail.head.tpe))) +: tail

    }


  // =====================================
  // Copy/Pasted from Scala 2.10.3. See SI-7694.
  private lazy val UncheckedBoundsClass = {
    try c.mirror.staticClass("scala.reflect.internal.annotations.uncheckedBounds")
    catch { case _: ScalaReflectionException => NoSymbol }
  }
  final def uncheckedBounds(tp: Type): Type = {
    if (tp.typeArgs.isEmpty || UncheckedBoundsClass == NoSymbol) tp
    else withAnnotation(tp, Annotation(UncheckedBoundsClass.asType.toType, Nil, ListMap()))
  }
  // =====================================  
}


