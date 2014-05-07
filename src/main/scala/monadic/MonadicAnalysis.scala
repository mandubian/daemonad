package categoric
package core

import scala.reflect.macros.Context

trait MonadicAnalysis extends CategoricAnalysis {

  import c.universe._

  /**
   * Analyze the contents of an `categoric` block in order to:
   * - Report unsupported `peek` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  override def reportUnsupportedPeeks(tree: Tree): Unit = {
    val analyzer = new UnsupportedPeekAnalyzer
    analyzer.traverse(tree)
    // analyzer.hasUnsupportedPeeks // XB: not used?!
  }

  private class UnsupportedPeekAnalyzer extends CategoricTraverser {
    var hasUnsupportedPeeks = false

    override def nestedClass(classDef: ClassDef) {
      val kind = if (classDef.symbol.asClass.isTrait) "trait" else "class"
      reportUnsupportedPeek(classDef, s"nested ${kind}")
    }

    override def nestedModule(module: ModuleDef) {
      reportUnsupportedPeek(module, "nested object")
    }

    override def nestedMethod(defDef: DefDef) {
      reportUnsupportedPeek(defDef, "nested method")
    }

    override def byNameArgument(arg: Tree) {
      reportUnsupportedPeek(arg, "by-name argument")
    }

    override def function(function: Function) {
      reportUnsupportedPeek(function, "nested function")
    }

    override def patMatFunction(tree: Match) {
      reportUnsupportedPeek(tree, "nested function")
    }

    override def traverse(tree: Tree) {
      def containsPeek = tree exists isPeekX
      tree match {
        case Try(_, _, _) if containsPeek                    =>
          reportUnsupportedPeek(tree, "try/catch")
          super.traverse(tree)
        case Return(_)                                        =>
          c.abort(tree.pos, "return is illegal within a async block")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY) =>
          // TODO lift this restriction
          c.abort(tree.pos, "lazy vals are illegal within an async block")
        case CaseDef(_, guard, _) if guard exists isPeekX     =>
          // TODO lift this restriction
          reportUnsupportedPeek(tree, "pattern guard")
        case _                                                =>
          super.traverse(tree)
      }
    }

    /**
     * @return true, if the tree contained an unsupported Peek.
     */
    private def reportUnsupportedPeek(tree: Tree, whyUnsupported: String): Boolean = {
      val badPeeks: List[RefTree] = tree collect {
        case rt: RefTree if isPeekX(rt) => rt
      }
      badPeeks foreach {
        tree =>
          reportError(tree.pos, s"Peek must not be used under a $whyUnsupported.")
      }
      badPeeks.nonEmpty
    }

    private def reportError(pos: Position, msg: String) {
      hasUnsupportedPeeks = true
      c.abort(pos, msg)
    }
  }
}
