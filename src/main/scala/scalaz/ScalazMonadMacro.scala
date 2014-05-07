package categoric
package scalaz

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import core._
import categoric.monadic._


trait ScalazMonadicTransform extends MonadicTransform with ScalazMonadicUtils {
  import c.universe._
  import Flag._
  import c.internal._
  import decorators._

  override def peekMethodSymbol: Symbol = {
    c.mirror.staticPackage("categoric.scalaz").typeSignature.member("peek": TermName)
  }

  override def peek2MethodSymbol: Symbol = {
    c.mirror.staticPackage("categoric.scalaz").typeSignature.member("peek2": TermName)
  }

  def inferMonadTransformer[M[_], N[_], T]
        (api: TypingTransformApi)
        (implicit mTag: WeakTypeTag[M[_]], nTag: WeakTypeTag[N[_]], tTag: WeakTypeTag[T])
        //(mType: Type, nType: Type, resultType: Type)
        : (Type, Tree => Tree => Tree, Tree => Tree) =
  {

    val wtpe = weakTypeOf[_root_.scalaz.Monad[({ type l[T] = _root_.scalaz.OptionT[M, T] })#l]]
    val mtpe = mTag.tpe.typeConstructor
    val wtpe2 = wtpe.map { _ match {
      case TypeRef(prefix, sym, args) if sym.isFreeType =>
        typeRef(NoPrefix, mtpe.typeSymbol, args)
      case tpe => tpe
    }}

    println(s"wtpe:$wtpe wtpe2:$wtpe2")
    val i = c.inferImplicitValue(wtpe2, silent = false)
    println(s"i:$i")

    val tpe = weakTypeOf[_root_.scalaz.OptionT[M, _]]
    val tpe2 = tpe.map { _ match {
      case TypeRef(prefix, sym, args) if sym.isFreeType =>
        typeRef(NoPrefix, mtpe.typeSymbol, args)
      case tpe => tpe
    }}

    val norm = tpe2.typeConstructor.normalize
    val PolyType(params, raw) = norm
    val customTpe = appliedType(raw, List(mTag.tpe.typeConstructor, tTag.tpe))

    val constructor = customTpe.member(nme.CONSTRUCTOR).asMethod
    val extractor = customTpe.member(newTermName("run")).asMethod

    println("const:"+constructor)
    def constr(tpe: Tree)(t: Tree) = q"${customTpe.typeSymbol.companion}[$mtpe, $tpe]($t)"
    def extr(t: Tree) = q"$t.$extractor"

    (wtpe2, constr, extr)
  }

}

object ScalazMonadMacro extends CategoricMacro {

  override def blockImpl[M[_], T: c0.WeakTypeTag](c0: Context)
                                                (body: c0.Expr[T])
                                                (implicit mw: c0.WeakTypeTag[M[_]]): c0.Expr[M[T]] = 
  {
    import c0.universe._, c0.internal._, decorators._

    val system = new ScalazMonadicTransform {
      val c: c0.type = c0
    }

    val code = system.transform[M, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[T])

    println("CODE:"+code)

    println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[T]](code)
  }

  override def blockImpl2[M[_], N[_], T: c0.WeakTypeTag](c0: Context)
                                                (body: c0.Expr[T])
                                                (implicit mw: c0.WeakTypeTag[M[_]],
                                                          nw: c0.WeakTypeTag[N[_]]): c0.Expr[M[N[T]]] = 
  {
    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadicTransform {
      val c: c0.type = c0
    }

    val code = transformer.transform2[M, N, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[N[_]], weakTypeTag[T])

    println("CODE:"+code)

    println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[N[T]]](code)
  }
}
