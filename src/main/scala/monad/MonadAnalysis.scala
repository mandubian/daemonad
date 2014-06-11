/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad

import scala.reflect.macros.Context
import core._


trait MonadAnalysis extends DAnalysis {

  import c.universe._

  def reportUnsupportedMonadTpe(monadTpes: List[TpeHelper], tree: Tree, tpe: TpeHelper): Unit

  /**
   * Analyze the contents of an `monadic` block in order to:
   * - Report unsupported `Snoop` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  override def reportUnsupportedSnoops(monadTpes: List[TpeHelper], tree: Tree): Unit = {
    val analyzer = new UnsupportedSnoopAnalyzer(monadTpes)
    analyzer.traverse(tree)
    // analyzer.hasUnsupportedSnoops // XB: not used?!
  }

  private class UnsupportedSnoopAnalyzer(monadTpes: List[TpeHelper]) extends DaemonadTraverser {
    val firstDepth = monadTpes.size

    var hasUnsupportedSnoops = false

    var firstSnoop: Option[Int] = None

    override def nestedClass(classDef: ClassDef) = {
      val kind = if (classDef.symbol.asClass.isTrait) "trait" else "class"
      reportUnsupportedSnoop(classDef, s"nested ${kind}")
    }

    override def nestedModule(module: ModuleDef) = {
      reportUnsupportedSnoop(module, "nested object")
    }

    override def nestedMethod(defDef: DefDef) = {
      reportUnsupportedSnoop(defDef, "nested method")
    }

    override def byNameArgument(arg: Tree) = {
      reportUnsupportedSnoop(arg, "by-name argument")
    }

    override def function(function: Function) = {
      reportUnsupportedSnoop(function, "nested function")
    }

    override def patMatFunction(tree: Match) = {
      reportUnsupportedSnoop(tree, "nested function")
    }

    override def traverse(tree: Tree) = {
      def containsSnoop = tree exists isSnoopX
      tree match {
        case Try(_, _, _) if containsSnoop                    =>
          reportUnsupportedSnoop(tree, "Snoop must not be used under a try/catch")
          super.traverse(tree)
        case Return(_)                                        =>
          c.abort(tree.pos, "Snoop must not be used under a return is illegal within a monadic block")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY) =>
          // TODO lift this restriction
          c.abort(tree.pos, "Snoop must not be used under a lazy vals are illegal within an monadic block")
        case CaseDef(_, guard, _) if guard exists isSnoopX     =>
          // TODO lift this restriction
          reportUnsupportedSnoop(tree, "Snoop must not be used under a pattern guard")

        case q"$fun[..$targs]($arg)" if isSnoopX(fun) =>
          reportUnsupportedMonadTpe(monadTpes, arg, TpeHelper(arg.tpe))
          if(firstSnoop.isEmpty) {
            val d = snoopDepth(tree)
            println(s"D:$d firstd:$firstDepth")
            if(d != firstDepth) {
              reportUnsupportedSnoop(tree, s"First call to `snoopX` is expected to be `snoop$firstDepth` and not `snoop${if(d==1) "" else d}`")
            }
            else { firstSnoop = Some(d) }
          }
          super.traverse(tree)

        case q"$mods val $value: $tpt = $arg" if !(arg exists isSnoopX) =>
          val ttpt = TpeHelper(c.typecheck(tpt, c.TYPEmode).tpe)
          if(ttpt.asType.typeArgs.size > 1) reportUnsupportedMonadTpe(monadTpes, tree, ttpt)
          super.traverse(tree)

        /*case q"$fun[..$targs](...$argss)" =>
          super.traverse(fun)
          argss.map(_.map(super.traverse(_)))*/
        case _ =>
          super.traverse(tree)
      }
    }

    /**
     * @return true, if the tree contained an unsupported Snoop.
     */
    private def reportUnsupportedSnoop(tree: Tree, whyUnsupported: String): Boolean = {
      val badSnoops: List[RefTree] = tree collect {
        case rt: RefTree if isSnoopX(rt) => rt
      }
      badSnoops foreach {
        tree =>
          reportError(tree.pos, s"$whyUnsupported.")
      }
      badSnoops.nonEmpty
    }

    private def reportError(pos: Position, msg: String) {
      hasUnsupportedSnoops = true
      c.abort(pos, msg)
    }
  }
}
