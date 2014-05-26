package categoric
package core

import scala.reflect.macros.Context

trait MonadicAnalysis extends CategoricAnalysis {

  import c.universe._

  /**
   * Analyze the contents of an `categoric` block in order to:
   * - Report unsupported `Snoop` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  override def reportUnsupportedSnoops(tree: Tree, firstDepth: Int): Unit = {
    val analyzer = new UnsupportedSnoopAnalyzer(firstDepth)
    analyzer.traverse(tree)
    // analyzer.hasUnsupportedSnoops // XB: not used?!
  }

  private class UnsupportedSnoopAnalyzer(firstDepth: Int) extends CategoricTraverser {
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
          c.abort(tree.pos, "Snoop must not be used under a return is illegal within a async block")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY) =>
          // TODO lift this restriction
          c.abort(tree.pos, "Snoop must not be used under a lazy vals are illegal within an async block")
        case CaseDef(_, guard, _) if guard exists isSnoopX     =>
          // TODO lift this restriction
          reportUnsupportedSnoop(tree, "Snoop must not be used under a pattern guard")
        case q"$fun[..$targs](...$argss)" if isSnoopX(fun) =>
          if(firstSnoop.isEmpty) {
            val d = snoopDepth(tree)
            if(d < firstDepth) {
              reportUnsupportedSnoop(tree, s"First call to `snoopX` is expected to be `snoop$firstDepth` and not `snoop${if(d==1) "" else d}`")
            }
            else { firstSnoop = Some(d) }
          }
          super.traverse(tree)
        case q"$fun[..$targs](...$argss)" =>
          super.traverse(fun)
          argss.map(_.map(super.traverse(_)))
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
