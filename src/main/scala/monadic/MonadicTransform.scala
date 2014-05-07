package categoric
package monadic

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import scala.language.experimental.macros

import core._

trait MonadicTransform
  extends CategoricTransform
  with    TransformUtils
  with    MonadicANF
  with    MonadicAnalysis
  with    MonadicUtils {

  import c.universe._
  import Flag._
  import c.internal._
  import decorators._
  import scala.collection.mutable.{Map => MMap}

  /*def matchBlock(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                (symMap: MMap[Symbol, Tree])
                : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (Block(stats, expr), others) =>
      val t = transformMultiple(api, mTypes, resultType)(symMap)(stats :+ expr)
      val tOthers = transformMultiple(api, mTypes, resultType)(symMap)(others)
      t ++ tOthers
  }

  def matchIf(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
             (symMap: MMap[Symbol, Tree])
             : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (tree@If(cond, thenp, elsep), others) =>
      val tThen = listToBlock(transformSingle(api, mTypes, resultType)(symMap)(thenp, List()))
      val tElse = listToBlock(transformSingle(api, mTypes, resultType)(symMap)(elsep, List()))
      treeCopy.If(tree, api.recur(cond), tThen, tElse) +: transformMultiple(api, mTypes, resultType)(symMap)(others)
  }

  def matchReplaceSymbols(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                         (symMap: MMap[Symbol, Tree])
                         : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (e, others) if (symMap contains e.symbol) =>
      val p = symMap(e.symbol)
      p match {
        case q"$_ val $value: $_ = $_" => 
          treeCopy.Ident(p, value) +: transformMultiple(api, mTypes, resultType)(symMap)(others)
        case tree => 
          val otherss = transformMultiple(api, mTypes, resultType)(symMap)(others)
          transformMultiple(api, mTypes, resultType)(symMap)(tree +: otherss)
          /*others match {
            case List() => transformMultiple(api, mTypes, resultType)(symMap)(List(tree))
            case _      => transformMultiple(api, mTypes, resultType)(symMap)(others.init :+ tree :+ others.last)
          }*/

      }
      /*treeCopy.Ident(p, p.symbol.name)*/
      //t +: transformMultiple(api, mTypes, resultType)(symMap)(others)
  }

  def matchPeek1(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                (symMap: scala.collection.mutable.Map[Symbol, Tree])
                : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (tree@q"$mods val $value: $tpt = $fun($arg)", others) if isPeek1(fun) =>
      // M Type
      val mType = mTypes(0)
      val normmTypes = mType.typeConstructor.normalize
      val PolyType(mParams, mRaw) = normmTypes
      val mTypesTpe = existentialAbstraction(mParams, mRaw)
      val mtTpe = appliedType(mRaw, List(resultType))
      val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))

      val param = defineParam(api)(name.peek, tree.pos, resultType)
      val mtTpt = appliedType(mRaw, tpt.symbol.asType.toType)

      val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

      symMap += (tree.symbol -> param)
      val t = transformMultiple(api, mTypes, resultType)(symMap)(others)

      val _inner = api.typecheck(listToBlock(t))

      if(_inner.tpe =:= definitions.UnitTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

        val inner = api.typecheck(listToBlock(t :+ q"()"))
        val code = api.typecheck(
          monadMap(mType)(refexpr)(
            q"""($param) =>
              ${ splicer(inner) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

      } else if(_inner.tpe =:= mtUnitTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

        //val inner = api.typecheck(listToBlock(t :+ q"implicitly[Monad[$mTypes]].point(())"))
        val code = api.typecheck(
          monadBind(mType)(refexpr)(
            q"""($param) =>
              ${ splicer(_inner) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

      } else if(_inner.tpe <:< mTypesTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

        val code = api.typecheck(
          monadBind(mType)(refexpr)(
            q"""($param) =>
              ${ splicer(_inner) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

      } else {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

        val code = api.typecheck(
          monadMap(mType)(refexpr)(
            q"""($param) =>
              ${ splicer(_inner) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

      }
  }

  def matchPeek2(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                (symMap: scala.collection.mutable.Map[Symbol, Tree])
                : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (tree@q"$mods val $value: $tpt = $fun($arg)", others) if isPeek2(fun) =>
      // N Type
      val nType = mTypes(1)
      val nNormTypes = nType.typeConstructor.normalize
      val PolyType(nParams, nRaw) = nNormTypes
      val nTypesTpe = existentialAbstraction(nParams, nRaw)
      val ntTpe = appliedType(nRaw, List(resultType))
      val ntUnitTpe = appliedType(nRaw, List(typeOf[Unit]))

      // M Type
      val mType = mTypes(0)
      val mNormTypes = mType.typeConstructor.normalize
      val PolyType(mParams, mRaw) = mNormTypes
      val mTypesTpe = existentialAbstraction(mParams, mRaw)
      val mntTpe = appliedType(mRaw, List(ntTpe))
      val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))

      val mParam = defineParam(api)(name.peek, tree.pos, resultType)
      val mParamRef = gen.mkAttributedRef(mParam.symbol).setType(resultType).setPos(tree.pos)

      //val nParam = defineParam(api)(name.peek, tree.pos, resultType)

      val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

      //symMap += tree.symbol -> q"$peekMethodSymbol($mParamRef)"
      symMap += tree.symbol -> q"$mParamRef"

      val inners = transformMultiple(api, mTypes, resultType)(symMap)(others)
      val innerBlock = api.typecheck(listToBlock(inners))

      val (tpe, constr, extr) = inferMonadTransformer(api)(mType, nType, resultType)
      println(s"monadT ($tpe, $constr, $extr)")

      /*val oos = if(others exists (_ exists isPeek2)) {
        val o = others.head
        val (first, second) = others.tail span {
          case q"$_ val $_: $_ = $fun($_)" if isPeek2(fun) => false
          case _ => true
        }
        first ++ (second.head +: o +: second.tail)
      } else others

      val inners = transformMultiple(api, mTypes, resultType)(symMap)(oos) //map ( api.recur(_) )
      symMap -= tree.symbol
      val retryInner = transformMultiple(api, mTypes, resultType)(symMap)(inners)
      val innerBlock = api.typecheck(listToBlock(retryInner))*/

      /*if(innerBlock.tpe =:= definitions.UnitTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

        val innerBlockUnit = api.typecheck(listToBlock(inners :+ q"()"))

        val outer = api.typecheck(
          monadMap(nType)(mParamRef)(
            q"""($nParam) =>
              ${ splicer(innerBlockUnit) }
            """
          )
        )

        val code = api.typecheck(
          monadMap(mType)(refexpr)(
            q"""($mParam) =>
              ${ splicer(outer) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mntTpe, code)) :+ valueRef

      } else if(innerBlock.tpe =:= mtUnitTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

        val outer = api.typecheck(
          monadBind(nType)(mParamRef)(
            q"""($nParam) =>
              ${ splicer(innerBlock) }
            """
          )
        )

        val code = api.typecheck(
          monadBind(mType)(refexpr)(
            q"""($mParam) =>
              ${ splicer(outer) }
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mntTpe, code)) :+ valueRef

      } else*/ if(innerBlock.tpe <:< mTypesTpe) {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mntTpe).setPos(tree.pos)

        /*val outer = api.typecheck(
          monadBind(nType)(mParamRef)(
            q"""($nParam) =>
              ${ splicer(innerBlock) }
            """
          )
        )*/

        val code = api.typecheck(
          monadTBind(tpe, constr, extr)(refexpr)(
            q"""($mParam) =>
              ${constr(splicer(innerBlock))}
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mntTpe, code)) :+ valueRef

      } else {
        val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mntTpe).setPos(tree.pos)

        /*val outer = api.typecheck(
          monadMap(nType)(mParamRef)(
            q"""($nParam) =>
              ${ splicer(innerBlock) }
            """
          )
        )*/

        val code = api.typecheck(
          monadTMap(tpe, constr, extr)(refexpr)(
            q"""($mParam) =>
              ${splicer(innerBlock)}
            """
          )
        )

        List(copyVal(api)(tree, mods, value, mntTpe, code)) :+ valueRef

      }
  }

  def matchDefault(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                  (symMap: MMap[Symbol, Tree])
                  : PartialFunction[(Tree, List[Tree]), List[Tree]] = 
  {
    case (e, others) =>
      api.default(e) +: transformMultiple(api, mTypes, resultType)(symMap)(others)
  }*/

  type Construct[M[_]] = _root_.scalaz.Monad[M]

  def transform[M[_], T](body: Tree)(mTag: WeakTypeTag[M[_]], tTag: WeakTypeTag[T]): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = c.WeakTypeTag[T](uncheckedBounds(tTag.tpe))

    reportUnsupportedPeeks(body)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()
    println("AnfTree:"+anfTree)

    typingTransform(anfTree){ (tree, api) =>

        def transformSingle(symMap: MMap[Symbol, Tree])
                           (tree: Tree, others: List[Tree])
                           : List[Tree] = tree match {
          case tree@q"$mods val $value: $tpt = $fun($arg)" if isPeek1(fun) =>
            val resultType = tTag.tpe
            // M Type
            val mType = mTag.tpe
            val normmTypes = mType.typeConstructor.normalize
            val PolyType(mParams, mRaw) = normmTypes
            val mTypesTpe = existentialAbstraction(mParams, mRaw)
            val mtTpe = appliedType(mRaw, List(resultType))
            val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))

            val param = defineParam(api)(name.peek, tree.pos, resultType)
            val mtTpt = appliedType(mRaw, tpt.symbol.asType.toType)

            val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

            symMap += (tree.symbol -> param)
            val t = transformMultiple(symMap)(others)

            val _inner = api.typecheck(listToBlock(t))

            if(_inner.tpe =:= definitions.UnitTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

              val inner = api.typecheck(listToBlock(t :+ q"()"))
              val code = api.typecheck(
                monadMap(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else if(_inner.tpe =:= mtUnitTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

              //val inner = api.typecheck(listToBlock(t :+ q"implicitly[Monad[$mTypes]].point(())"))
              val code = api.typecheck(
                monadBind(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else if(_inner.tpe <:< mTypesTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

              val code = api.typecheck(
                monadBind(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

              val code = api.typecheck(
                monadMap(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            }

          case Block(stats, expr) =>
            val t = transformMultiple(symMap)(stats :+ expr)
            val tOthers = transformMultiple(symMap)(others)
            t ++ tOthers

          case tree@If(cond, thenp, elsep) =>
            val tThen = listToBlock(transformSingle(symMap)(thenp, List()))
            val tElse = listToBlock(transformSingle(symMap)(elsep, List()))
            treeCopy.If(tree, api.recur(cond), tThen, tElse) +: transformMultiple(symMap)(others)

          case e if (symMap contains e.symbol) =>
            val p = symMap(e.symbol)
            p match {
              case q"$_ val $value: $_ = $_" => 
                treeCopy.Ident(p, value) +: transformMultiple(symMap)(others)
              case tree => 
                val otherss = transformMultiple(symMap)(others)
                transformMultiple(symMap)(tree +: otherss)
            }

          case e =>
            api.default(e) +: transformMultiple(symMap)(others)
        }

        /*(
          matchBlock(api, mTypes, resultType)(symMap)
          orElse matchPeek1(api, mTypes, resultType)(symMap)
          orElse matchIf(api, mTypes, resultType)(symMap)
          orElse matchReplaceSymbols(api, mTypes, resultType)(symMap)
          orElse matchDefault(api, mTypes, resultType)(symMap)
        )(tree, others)*/

      def transformMultiple(symMap: MMap[Symbol, Tree])
                   (stats: List[Tree])
                   : List[Tree] = {
        stats match {
          case List() => List()
          case head :: tail => transformSingle(symMap)(head, tail)
        }
      }

      val t = transformSingle(symMap)(tree, List())
      t match {
        case List(e) => e
        case _ :: _  => listToBlock(t)
      }

    }
  }

  def transform2[M[_], N[_], T](body: Tree)(mTag: WeakTypeTag[M[_]], nTag: WeakTypeTag[N[_]], tTag: WeakTypeTag[T]): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = c.WeakTypeTag[T](uncheckedBounds(tTag.tpe))

    reportUnsupportedPeeks(body)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()
    println("AnfTree:"+anfTree)

      val resultType = tTag.tpe

      // N Type
      val nType = nTag.tpe
      val nNormTypes = nType.typeConstructor.normalize
      val PolyType(nParams, nRaw) = nNormTypes
      val nTypesTpe = existentialAbstraction(nParams, nRaw)
      val ntTpe = appliedType(nRaw, List(resultType))
      val ntUnitTpe = appliedType(nRaw, List(definitions.UnitTpe))

      // M Type
      val mType = mTag.tpe
      val mNormTypes = mType.typeConstructor.normalize
      val PolyType(mParams, mRaw) = mNormTypes
      val mTypesTpe = existentialAbstraction(mParams, mRaw)
      val mtTpe = appliedType(mRaw, List(resultType))

      // M[N] Type
      val mntTpe = appliedType(mRaw, List(ntTpe))
      val mtUnitTpe = appliedType(mRaw, List(definitions.UnitTpe))
      val mntUnitTpe = appliedType(mRaw, List(ntUnitTpe))


    typingTransform(anfTree){ (tree, api) =>

      def transformSingle(symMap: MMap[Symbol, Tree])
                         (tree: Tree, others: List[Tree])
                         : List[Tree] = tree match {

        case tree@q"$mods val $value: $tpt = $fun($arg)" if isPeek2(fun) =>

          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
          val mParam = defineParam(api)(name.peek, tree.pos, ttpt)
          val mParamRef = gen.mkAttributedRef(mParam.symbol).setType(resultType).setPos(tree.pos)

          //val nParam = defineParam(api)(name.peek, tree.pos, resultType)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          //symMap += tree.symbol -> q"$peekMethodSymbol($mParamRef)"
          symMap += tree.symbol -> q"$mParamRef"

          val inners = transformMultiple(symMap)(others)
          val innerBlock = api.typecheck(listToBlock(inners))

          val (tpe, constr, extr) = inferMonadTransformer[M, N, T](api)(mTag, nTag, tTag)

          //println(s"monadT ($tpe, $constr, $extr)")

          /*val oos = if(others exists (_ exists isPeek2)) {
            val o = others.head
            val (first, second) = others.tail span {
              case q"$_ val $_: $_ = $fun($_)" if isPeek2(fun) => false
              case _ => true
            }
            first ++ (second.head +: o +: second.tail)
          } else others

          val inners = transformMultiple(api, mTypes, resultType)(symMap)(oos) //map ( api.recur(_) )
          symMap -= tree.symbol
          val retryInner = transformMultiple(api, mTypes, resultType)(symMap)(inners)
          val innerBlock = api.typecheck(listToBlock(retryInner))*/

          if(innerBlock.tpe =:= definitions.UnitTpe) {
            val innerBlockUnit = api.typecheck(listToBlock(inners :+ q"()"))

            val code = api.typecheck(
              monadTMap(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${splicer(innerBlockUnit)}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(innerBlock.tpe =:= mntUnitTpe) {
            val code = api.typecheck(
              monadTBind(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${constr(tq"${definitions.UnitTpe}")(splicer(innerBlock))}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(innerBlock.tpe =:= ntUnitTpe) {
            val code = api.typecheck(
              monadTBind(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${constr(tq"${definitions.UnitTpe}")(monadPoint(mType)(splicer(innerBlock)))}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(innerBlock.tpe <:< mTypesTpe) {

            val code = api.typecheck(
              monadTBind(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${constr(tpt)(splicer(innerBlock))}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(innerBlock.tpe <:< nTypesTpe) {

            val code = api.typecheck(
              monadTBind(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${constr(tpt)(monadPoint(mType)(splicer(innerBlock)))}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else {
            val code = api.typecheck(
              monadTMap(tpe, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${splicer(innerBlock)}
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          }

        case tree@q"$mods val $value: $tpt = $fun($arg)" if(isPeek1(fun) && arg.tpe <:< nTypesTpe) =>
          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe

          val param = defineParam(api)(name.peek, tree.pos, ttpt)
          val ntTpt = appliedType(nRaw, tpt.symbol.asType.toType)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += (tree.symbol -> param)
          val t = transformMultiple(symMap)(others)

          val _inner = api.typecheck(listToBlock(t))

          if(_inner.tpe =:= definitions.UnitTpe) {
            val inner = api.typecheck(listToBlock(t :+ q"()"))
            val code = api.typecheck(
              monadMap(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(inner) }
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(_inner.tpe =:= ntUnitTpe) {
            val code = api.typecheck(
              monadBind(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(_inner) }
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else if(_inner.tpe <:< nTypesTpe) {
            val code = api.typecheck(
              monadBind(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(_inner) }
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          } else {
            val code = api.typecheck(
              monadMap(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(_inner) }
                """
              )
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

          }

        case Block(stats, expr) =>
          val t = transformMultiple(symMap)(stats :+ expr)
          val tOthers = transformMultiple(symMap)(others)
          t ++ tOthers

        case tree@If(cond, thenp, elsep) =>
          val tThen = listToBlock(transformSingle(symMap)(thenp, List()))
          val tElse = listToBlock(transformSingle(symMap)(elsep, List()))
          treeCopy.If(tree, api.recur(cond), tThen, tElse) +: transformMultiple(symMap)(others)

        case e if (symMap contains e.symbol) =>
          val p = symMap(e.symbol)
          p match {
            case q"$_ val $value: $_ = $_" =>
              treeCopy.Ident(p, value) +: transformMultiple(symMap)(others)
            case tree =>
              val otherss = transformMultiple(symMap)(others)
              transformMultiple(symMap)(tree +: otherss)
          }

        case e =>
          api.default(e) +: transformMultiple(symMap)(others)
      }

        /*(
          matchBlock(api, mTypes, resultType)(symMap)
          orElse matchPeek1(api, mTypes, resultType)(symMap)
          orElse matchIf(api, mTypes, resultType)(symMap)
          orElse matchReplaceSymbols(api, mTypes, resultType)(symMap)
          orElse matchDefault(api, mTypes, resultType)(symMap)
        )(tree, others)*/

      def transformMultiple(symMap: MMap[Symbol, Tree])
                   (stats: List[Tree])
                   : List[Tree] = {
        stats match {
          case List() => List()
          case head :: tail => transformSingle(symMap)(head, tail)
        }
      }

      val t = transformSingle(symMap)(tree, List())
      t match {
        case List(e) => e
        case _ :: _  => listToBlock(t)
      }

    }
  }

  /*def transform[M[_], T](body: Tree)(mTypes: WeakTypeTag[M[_]], resultType: WeakTypeTag[T]): Tree = {
    // Eugene Magic
    val normmTypes = mTypes.tpe.typeConstructor.normalize
    val PolyType(mParams, mRaw) = normmTypes
    val mTypesTpe = existentialAbstraction(mParams, mRaw)
    val mtTpe = appliedType(mRaw, List(resultType.tpe))
    val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))
    // Eugene Magic

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = c.WeakTypeTag[T](uncheckedBounds(resultType.tpe))

    reportUnsupportedPeeks(body)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()
    println("AnfTree:"+anfTree)

    typingTransform(anfTree){ (tree, api) =>

      def transformSingle(tree: Tree, others: List[Tree]): List[Tree] = {

        tree match {

          case Block(stats, expr) =>
            val t = transformMultiple(stats :+ expr)
            val tOthers = transformMultiple(others)
            t ++ tOthers

          case q"$mods val $value: $tpt = $fun($arg)" if isPeek1(fun) =>
            val param = defineParam(api)(name.peek, tree.pos, resultType.tpe)
            val mtTpt = appliedType(mRaw, tpt.symbol.asType.toType)

            val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

            symMap += (tree.symbol -> param)
            val t = transformMultiple(others)

            val _inner = api.typecheck(listToBlock(t))

            if(_inner.tpe =:= definitions.UnitTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

              val inner = api.typecheck(listToBlock(t :+ q"()"))
              val code = api.typecheck(
                monadMap(mTypes.tpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else if(_inner.tpe =:= mtUnitTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtUnitTpe).setPos(tree.pos)

              //val inner = api.typecheck(listToBlock(t :+ q"implicitly[Monad[$mTypes]].point(())"))
              val code = api.typecheck(
                monadBind(mTypes.tpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else if(_inner.tpe <:< mTypesTpe) {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

              val code = api.typecheck(
                monadBind(mTypes.tpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            } else {
              val valueRef = gen.mkAttributedStableRef(tree.symbol).setType(mtTpe).setPos(tree.pos)

              val code = api.typecheck(
                monadMap(mTypes.tpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              )

              List(copyVal(api)(tree, mods, value, mtTpt, code)) :+ valueRef

            }

          case If(cond, thenp, elsep) =>
            val tThen = listToBlock(transformSingle(thenp, List()))
            val tElse = listToBlock(transformSingle(elsep, List()))
            treeCopy.If(tree, api.recur(cond), tThen, tElse) +: transformMultiple(others)

          case Match(scrut, cases) =>
            val caseDefs = cases map {
              case CaseDef(pat, guard, body) =>
                val tBody = listToBlock(transformSingle(body, List()))
                treeCopy.CaseDef(tree, api.recur(pat), api.recur(guard), tBody)
            }
            treeCopy.Match(tree, api.recur(scrut), caseDefs) +: transformMultiple(others)

          case e if (symMap contains e.symbol) =>
            val p = symMap(e.symbol)
            treeCopy.Ident(p, p.symbol.name)  +: transformMultiple(others)

          case e =>
            api.default(e) +: transformMultiple(others)
        }
      }

      val t = transformSingle(tree, List())
      t match {
        case List(e) => e
        case _ :: _  => listToBlock(t)
      }

    }
  }*/

}


