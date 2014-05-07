package categoric
package core

import scala.reflect.macros.Context
import scala.reflect.api.Universe

trait TransformUtils extends CategoricContext {

  import c.universe._
  import c.internal._
  import decorators._
  import Flag._

  import scala.collection.immutable.ListMap

  def isPeekX(fun: Tree): Boolean = fun.symbol == peekMethodSymbol || fun.symbol == peek2MethodSymbol
  def isPeek1(fun: Tree): Boolean = fun.symbol == peekMethodSymbol
  def isPeek2(fun: Tree): Boolean = fun.symbol == peek2MethodSymbol

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
    val sym = api.currentOwner.newTermSymbol(name.fresh(prefix), pos, SYNTHETIC).setInfo(uncheckedBounds(lhs.tpe))
    lhs.changeOwner(api.currentOwner, sym)
    valDef(sym, lhs.changeOwner(api.currentOwner, sym)).setType(NoType).setPos(pos)
  }

  def defineParam(api: TypingTransformApi)(prefix: String, pos: Position, tpe: Type): ValDef = {
    val sym = api.currentOwner.newTermSymbol(c.freshName(prefix), pos, PARAM).setInfo(tpe)
    valDef(sym).setType(tpe).setPos(pos)
  }

  def copyVal(api: TypingTransformApi)(tree: Tree, mods: Modifiers, name: Name, tpt: Type, rhs: Tree): ValDef = {
    val res = treeCopy.ValDef(tree, mods, name, tq"$tpt", rhs)
    res.setSymbol(tree.symbol.setInfo(tpt))
    res
  }


  object name {
    val peek          = "peek"
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
  trait CategoricTraverser extends Traverser {
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
          val isInByName = isByName(fun)
          for ((args, i) <- argss.zipWithIndex) {
            for ((arg, j) <- args.zipWithIndex) {
              if (!isInByName(i, j)) traverse(arg)
              else byNameArgument(arg)
            }
          }
          traverse(fun)
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


