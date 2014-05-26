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
  with    MonadicUtils
  with    MonadicUpstack {

  import c.universe._
  import Flag._
  import c.internal._
  import decorators._
  import scala.collection.mutable.{Map => MMap}

  type Construct[M[_]] = _root_.scalaz.Monad[M]

  override def transform(body: Tree)(monadTpes: List[Type], resultType: Type): Tree = {
    // TODO improve this report
    reportUnsupportedSnoops(body, monadTpes.size)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()

    val monadTpeHelpers = monadTpes map TpeHelper
    val resultTpeHelper = TpeHelper(resultType)

    val aliasTypeMap: scala.collection.mutable.ListBuffer[AliasTpe] = scala.collection.mutable.ListBuffer()

    val transformed: Tree = typingTransform(anfTree){ (tree, api) =>

      def buildCode(tpeStack: List[TpeHelper], tType: TpeHelper, value: Symbol, tpt: Tree, arg: Tree, others: List[Tree]): Tree = {

        val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
        val param = defineParam(api)(name.Snoop, tree.pos, ttpt)
        val paramRef = gen.mkAttributedRef(param.symbol).setType(ttpt).setPos(tree.pos)

        val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

        symMap += value -> q"$paramRef"

        val inners = transformMultiple(others)
        val innerBlock = api.typecheck(listToBlock(inners))

        tpeStack match {
          case List(mTpe) =>
            val monadTree =
              if(innerBlock.tpe <:< mTpe.existential) {
                monadBind(mTpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(innerBlock) }
                  """
                )
              } else {
                monadMap(mTpe)(refexpr)(
                  q"""($param) =>
                    ${ splicer(innerBlock) }
                  """
                )
              }
            api.typecheck(monadTree)

          case _ =>
            val AliasTpe(aliasType, aliasName, aliasTrees, constr, extr, refTpe) = inferMonadTransformerTpe3(api, tpeStack, aliasTypeMap)
            val monadTree = {
              val upstacked = api.typecheck(upstack((tpeStack :+ tType), innerBlock, true))

              if(upstacked.isEmpty) {

                monadTMap(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($param) =>
                    ${splicer(innerBlock)}
                  """
                )

              } else {
                val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($param) =>
                    ${constr(ctpe)(splicer(upstacked))}
                  """
                )

              }
            }
            val block = api.typecheck(
              q"""{
                ..$aliasTrees
                ${extr(monadTree)}
              }"""
            )

            println(s"""
####################################################
  $monadTree
----------------------------------------------------
  $block
----------------------------------------------------
  ${blockToList(block).last}
####################################################
            """)
            //blockToList(block).last
            block

        }

      }

      def transformSingle(tree: Tree, others: List[Tree])
                         : List[Tree] = tree match {

        case tree@q"$mods val $value: $tpt = $fun($arg)" if(isSnoopX(fun)) =>
          val code =
            if      (isSnoop1(fun)) buildCode(monadStackFromSnoopX(fun, monadTpeHelpers), resultTpeHelper, tree.symbol, tpt, arg, others)
            else if (isSnoop2(fun)) buildCode(monadStackFromSnoopX(fun, monadTpeHelpers), resultTpeHelper, tree.symbol, tpt, arg, others)
            else if (isSnoop3(fun)) buildCode(monadStackFromSnoopX(fun, monadTpeHelpers), resultTpeHelper, tree.symbol, tpt, arg, others)
            else                    c.abort(c.enclosingPosition, s"Can't manage this snoop level $fun")

          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue, newValueRef)

        case Block(stats, expr) =>
          val t = transformMultiple(stats :+ expr)
          val tOthers = transformMultiple(others)
          t ++ tOthers

        case tree@If(cond, thenp, elsep) =>
          val tThen = listToBlock(transformSingle(thenp, List()))
          val tElse = listToBlock(transformSingle(elsep, List()))
          treeCopy.If(tree, api.recur(cond), tThen, tElse) +: transformMultiple(others)

        case e if (symMap contains e.symbol) =>
          val p = symMap(e.symbol)
          p match {
            case q"$_ val $value: $_ = $_" =>
              treeCopy.Ident(p, value) +: transformMultiple(others)
            case tree =>
              val otherss = transformMultiple(others)
              transformMultiple(tree +: otherss)
          }

        case e =>
          api.default(e) +: transformMultiple(others)
      }

      def transformMultiple(stats: List[Tree]): List[Tree] = {
        stats match {
          case List() => List()
          case head :: tail => transformSingle(head, tail)
        }
      }

      transformSingle(tree, List()) match {
        case List(e) => e
        case t  => listToBlock(t)
      }
    }

    transformed
    // val tmap = aliasTypeMap flatMap { t => t.trees} toSeq

    // println("TMAP:"+tmap)
    // val code = blockToList(transformed)
    // q"""{
    //   ..${ tmap }
    //   ..${ code }
    // }"""

  }


  /*def transform1[M[_], T](body: Tree)(mTag: WeakTypeTag[M[_]], tTag: WeakTypeTag[T]): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = c.WeakTypeTag[T](uncheckedBounds(tTag.tpe))

    reportUnsupportedSnoops(body, 1)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()

    val resultType = tTag.tpe
    // M Type
    val mType = mTag.tpe
    val normmTypes = mType.typeConstructor.normalize
    val PolyType(mParams, mRaw) = normmTypes
    val mTypesTpe = existentialAbstraction(mParams, mRaw)
    val mtTpe = appliedType(mRaw, List(resultType))
    val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))

    typingTransform(anfTree){ (tree, api) =>

        def transformSingle(symMap: MMap[Symbol, Tree])
                           (tree: Tree, others: List[Tree])
                           : List[Tree] = tree match {
          case tree@q"$mods val $value: $tpt = $fun($arg)" if isSnoop1(fun) =>

            val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
            val param = defineParam(api)(name.Snoop, tree.pos, ttpt)
            val paramRef = gen.mkAttributedRef(param.symbol).setType(ttpt).setPos(tree.pos)

            val mtTpt = appliedType(mRaw, tpt.symbol.asType.toType)

            val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

            symMap += (tree.symbol -> q"$paramRef")
            val t = transformMultiple(symMap)(others)

            val _inner = api.typecheck(listToBlock(t))

            val code = api.typecheck(
              if(_inner.tpe <:< mTypesTpe) {
                monadBind(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              } else {
                monadMap(mType)(refexpr)(
                  q"""($param) =>
                    ${ splicer(_inner) }
                  """
                )
              }
            )

            val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
            val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
            List(newValue) :+ newValueRef

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
          orElse matchSnoop1(api, mTypes, resultType)(symMap)
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
  }*/

  /*def transform2[M[_], N[_], T](body: Tree)(mTag: WeakTypeTag[M[_]], nTag: WeakTypeTag[N[_]], tTag: WeakTypeTag[T]): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = c.WeakTypeTag[T](uncheckedBounds(tTag.tpe))

    reportUnsupportedSnoops(body, 2)

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

        case tree@q"$mods val $value: $tpt = $fun($arg)" if isSnoop2(fun) =>

          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
          val mParam = defineParam(api)(name.Snoop, tree.pos, ttpt)
          val mParamRef = gen.mkAttributedRef(mParam.symbol).setType(ttpt).setPos(tree.pos)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += tree.symbol -> q"$mParamRef"

          val inners = transformMultiple(symMap)(others)
          val innerBlock = api.typecheck(listToBlock(inners))

          //val (tpe, constr, extr) = inferMonadTransformerTpe(api)(mTag.tpe, nTag.tpe, tTag.tpe)
          val (impl, constr, extr) = inferMonadTransformerTpe2(api)(mTag.tpe :: nTag.tpe :: Nil)

          println("INFER:"+inferMonadTransformerTpe2(api)( mTag.tpe :: typeTag[List[_]].tpe.typeConstructor :: nTag.tpe :: Nil))
          val code = api.typecheck(
            if(innerBlock.tpe <:< mTypesTpe) {

              val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

              monadTBind(impl, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                q"""($mParam) =>
                  ${constr(ctpe)(splicer(innerBlock))}
                """
              )

            } else {

              val upstacked = api.typecheck(upstack(mType :: nType :: resultType :: Nil, innerBlock, true))

              if(upstacked.isEmpty) {

                monadTMap(impl, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                  q"""($mParam) =>
                    ${splicer(innerBlock)}
                  """
                )

              } else {

                println(s"#### upstacked:$upstacked innerBlock.tpe:${innerBlock.tpe}")
                val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                monadTBind(impl, arg.tpe, innerBlock.tpe, constr(tpt), extr)(refexpr)(
                  q"""($mParam) =>
                    ${constr(ctpe)(splicer(upstacked))}
                  """
                )

              }
            }
          )

          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue) :+ newValueRef

        case tree@q"$mods val $value: $tpt = $fun($arg)" if(isSnoop1(fun) /*&& arg.tpe <:< nTypesTpe*/) =>
          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe

          val param = defineParam(api)(name.Snoop, tree.pos, ttpt)
          val ntTpt = appliedType(nRaw, tpt.symbol.asType.toType)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += (tree.symbol -> param)
          val t = transformMultiple(symMap)(others)

          val _inner = api.typecheck(listToBlock(t))

          val code = api.typecheck(
            if(_inner.tpe <:< nTypesTpe) {
              monadBind(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(_inner) }
                """
              )
            } else {
              monadMap(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(_inner) }
                """
              )
            }
          )

          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue) :+ newValueRef

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
          orElse matchSnoop1(api, mTypes, resultType)(symMap)
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
  }*/
  /*def transform3(body: Tree)(lTag: Type, mTag: Type, nTag:Type, tTag: Type): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    //implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = tTag(uncheckedBounds(tTag.tpe))

    reportUnsupportedSnoops(body, 2)

    val anfTree = anfTransform(body)

    val symMap: scala.collection.mutable.Map[Symbol, Tree] = scala.collection.mutable.Map()

    val resultType = tTag

    // L Type
    val lType = lTag
    val lNormTypes = lType.typeConstructor.normalize
    val PolyType(lParams, lRaw) = lNormTypes
    val lTypesTpe = existentialAbstraction(lParams, lRaw)
    val ltTpe = appliedType(lRaw, List(resultType))
    val ltUnitTpe = appliedType(lRaw, List(definitions.UnitTpe))

    // M Type
    val mType = mTag
    val mNormTypes = mType.typeConstructor.normalize
    val PolyType(mParams, mRaw) = mNormTypes
    val mTypesTpe = existentialAbstraction(mParams, mRaw)
    val mtTpe = appliedType(mRaw, List(resultType))

    // N Type
    val nType = nTag
    val nNormTypes = nType.typeConstructor.normalize
    val PolyType(nParams, nRaw) = nNormTypes
    val nTypesTpe = existentialAbstraction(nParams, nRaw)
    val ntTpe = appliedType(nRaw, List(resultType))
    val ntUnitTpe = appliedType(nRaw, List(definitions.UnitTpe))

    // M[N] Type
    val mntTpe = appliedType(mRaw, List(ntTpe))
    val mtUnitTpe = appliedType(mRaw, List(definitions.UnitTpe))
    val mntUnitTpe = appliedType(mRaw, List(ntUnitTpe))

    typingTransform(anfTree){ (tree, api) =>

      def transformSingle(symMap: MMap[Symbol, Tree])
                         (tree: Tree, others: List[Tree])
                         : List[Tree] = tree match {

        case tree@q"$mods val $value: $tpt = $fun($arg)" if isSnoop3(fun) =>
          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
          val lParam = defineParam(api)(name.Snoop, tree.pos, ttpt)
          val lParamRef = gen.mkAttributedRef(lParam.symbol).setType(ttpt).setPos(tree.pos)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += tree.symbol -> q"$lParamRef"

          val inners = transformMultiple(symMap)(others)
          val innerBlock = api.typecheck(listToBlock(inners))

          val (aliasType, aliasName, aliasTrees, constr, extr) = inferMonadTransformerTpe3(api)(lType :: mType :: nType :: Nil)

          val monadTree =
            if(innerBlock.tpe <:< lTypesTpe) {
              val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

              monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                q"""($lParam) =>
                  ${constr(ctpe)(splicer(innerBlock))}
                """
              )
            } else {
              val upstacked = api.typecheck(upstack(lType :: mType :: nType :: resultType :: Nil, innerBlock, true))

              if(upstacked.isEmpty) {

                monadTMap(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($lParam) =>
                    ${splicer(innerBlock)}
                  """
                )

              } else {
                val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($lParam) =>
                    ${constr(ctpe)(splicer(upstacked))}
                  """
                )
              }
            }

          val code = api.typecheck(
            extr(q"""{
              ..$aliasTrees
              $monadTree
            }""")
          )
          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue) :+ newValueRef


        case tree@q"$mods val $value: $tpt = $fun($arg)" if isSnoop2(fun) =>

          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe
          val mParam = defineParam(api)(name.Snoop, tree.pos, ttpt)
          val mParamRef = gen.mkAttributedRef(mParam.symbol).setType(ttpt).setPos(tree.pos)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += tree.symbol -> q"$mParamRef"

          val inners = transformMultiple(symMap)(others)
          val innerBlock = api.typecheck(listToBlock(inners))

          //val (tpe, constr, extr) = inferMonadTransformerTpe(api)(mTag.tpe, nTag.tpe, tTag.tpe)
          //val (impl, constr, extr) = inferMonadTransformerTpe2(api)(mTag.tpe :: nTag.tpe :: Nil)
          val (aliasType, aliasName, aliasTrees, constr, extr) = inferMonadTransformerTpe3(api)(mType :: nType :: Nil)

          val monadTree =
            if(innerBlock.tpe <:< mTypesTpe) {
              val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

              monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                q"""($mParam) =>
                  ${constr(ctpe)(splicer(innerBlock))}
                """
              )
            } else {
              val upstacked = api.typecheck(upstack(mType :: nType :: resultType :: Nil, innerBlock, true))

              if(upstacked.isEmpty) {

                monadTMap(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($mParam) =>
                    ${splicer(innerBlock)}
                  """
                )

              } else {
                val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(
                  q"""($mParam) =>
                    ${constr(ctpe)(splicer(upstacked))}
                  """
                )
              }
            }

          val code = api.typecheck(
            extr(q"""{
              ..$aliasTrees
              $monadTree
            }""")
          )

          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue) :+ newValueRef

        case tree@q"$mods val $value: $tpt = $fun($arg)" if(isSnoop1(fun) /*&& arg.tpe <:< nTypesTpe*/) =>
          val ttpt = c.typecheck(tpt, c.TYPEmode).tpe

          val param = defineParam(api)(name.Snoop, tree.pos, ttpt)
          val ntTpt = appliedType(nRaw, tpt.symbol.asType.toType)

          val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

          symMap += (tree.symbol -> param)
          val inners = transformMultiple(symMap)(others)
          val innerBlock = api.typecheck(listToBlock(inners))

          val monadTree =
            if(innerBlock.tpe <:< nTypesTpe) {
              monadBind(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(innerBlock) }
                """
              )
            } else {
              monadMap(nType)(refexpr)(
                q"""($param) =>
                  ${ splicer(innerBlock) }
                """
              )
            }
          val code = api.typecheck(monadTree)

          val newValue = copyVal(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(tree.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue) :+ newValueRef

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
  }*/


}


// CODE TRIES KEPT IN CASE OF... :)

        /*(
          matchBlock(api, mTypes, resultType)(symMap)
          orElse matchSnoop1(api, mTypes, resultType)(symMap)
          orElse matchIf(api, mTypes, resultType)(symMap)
          orElse matchReplaceSymbols(api, mTypes, resultType)(symMap)
          orElse matchDefault(api, mTypes, resultType)(symMap)
        )(tree, others)*/

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

  def matchSnoop1(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                (symMap: scala.collection.mutable.Map[Symbol, Tree])
                : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (tree@q"$mods val $value: $tpt = $fun($arg)", others) if isSnoop1(fun) =>
      // M Type
      val mType = mTypes(0)
      val normmTypes = mType.typeConstructor.normalize
      val PolyType(mParams, mRaw) = normmTypes
      val mTypesTpe = existentialAbstraction(mParams, mRaw)
      val mtTpe = appliedType(mRaw, List(resultType))
      val mtUnitTpe = appliedType(mRaw, List(typeOf[Unit]))

      val param = defineParam(api)(name.Snoop, tree.pos, resultType)
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

  def matchSnoop2(api: TypingTransformApi, mTypes: List[Type], resultType: Type)
                (symMap: scala.collection.mutable.Map[Symbol, Tree])
                : PartialFunction[(Tree, List[Tree]), List[Tree]] =
  {
    case (tree@q"$mods val $value: $tpt = $fun($arg)", others) if isSnoop2(fun) =>
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

      val mParam = defineParam(api)(name.Snoop, tree.pos, resultType)
      val mParamRef = gen.mkAttributedRef(mParam.symbol).setType(resultType).setPos(tree.pos)

      //val nParam = defineParam(api)(name.Snoop, tree.pos, resultType)

      val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

      //symMap += tree.symbol -> q"$SnoopMethodSymbol($mParamRef)"
      symMap += tree.symbol -> q"$mParamRef"

      val inners = transformMultiple(api, mTypes, resultType)(symMap)(others)
      val innerBlock = api.typecheck(listToBlock(inners))

      val (tpe, constr, extr) = inferMonadTransformer(api)(mType, nType, resultType)
      println(s"monadT ($tpe, $constr, $extr)")

      /*val oos = if(others exists (_ exists isSnoop2)) {
        val o = others.head
        val (first, second) = others.tail span {
          case q"$_ val $_: $_ = $fun($_)" if isSnoop2(fun) => false
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
