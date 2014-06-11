/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad

import scala.reflect.macros.Context
import scala.reflect.api.Universe

import scala.language.experimental.macros

import core._


trait MonadTransform
  extends DTransform
  with    MonadANF
  with    MonadAnalysis
  with    MonadUtils
  with    MonadUpstack
  with    TransformUtils
  with    PrinterUtils {

  import c.universe._
  import Flag._
  import c.internal._
  import decorators._
  import scala.collection.mutable.{Map => MMap}

  type Construct[M[_]] = _root_.scalaz.Monad[M]

  override def transform(body: Tree)(monadTpes: List[Type], resultTpe: Type): Tree = {
    val monadTpeHelpers = monadTpes map TpeHelper
    val resultTpeHelper = TpeHelper(resultTpe)

    // TODO improve this report
    reportUnsupportedSnoops(monadTpeHelpers, body)

    val anfTree = anfTransform(body)

    vprintln("-----> ANF TREE:"+anfTree)

    val symMap: scala.collection.mutable.Map[Symbol, (Tree, TpeHelper)] = scala.collection.mutable.Map()

    val aliases: scala.collection.mutable.ListBuffer[AliasTpe] = scala.collection.mutable.ListBuffer()

    val transformed: Tree = typingTransform(anfTree){ (tree, api) =>

      def buildCode(tpeStack: List[TpeHelper], tType: TpeHelper, value: Symbol, tpt: Tree, arg: Tree, others: List[Tree]): Tree = {

        val ttpt = TpeHelper(c.typecheck(tpt, c.TYPEmode).tpe)
        val refexpr = gen.mkAttributedRef(arg.symbol).setType(arg.tpe).setPos(tree.pos)

        val (aliasTpe, param, paramRef) = tpeStack match {
          case List(_) =>
            val param = defineParam(api)(name.Snoop, tree.pos, ttpt.asType)
            val paramRef = gen.mkAttributedRef(param.symbol).setType(ttpt.asType).setPos(tree.pos)
            symMap += value -> (q"$paramRef", ttpt)
            (None, param, paramRef)

          case _ =>
            val aliasTpe = inferMonadTransformerTpe(api, aliases)(tpeStack, ttpt)
            val param = defineParam(api)(name.Snoop, tree.pos, ttpt.asType)
            val paramRef = gen.mkAttributedRef(param.symbol).setType(ttpt.asType).setPos(tree.pos)
            symMap += value -> (q"$paramRef", ttpt)
            (Some(aliasTpe), param, paramRef)
        }

        val inners = transformMultiple(others)
        val innerBlock = api.typecheck(listToBlock(inners))

        aliasTpe match {
          case None =>

            val code = api.typecheck(q"""{ ($param) =>
              ${splicer(innerBlock)}
            }""")

            // VERY IMPORTANT TO RELOCATE OWNERS
            innerBlock.changeOwner(value.owner, code.symbol)

            val monadTree =
              if(innerBlock.tpe <:< tpeStack.head.existential) {
                monadBind(tpeStack.head)(refexpr)(code)
              } else {
                monadMap(tpeStack.head)(refexpr)(code)
              }
            api.typecheck(monadTree)

          case Some(AliasTpe(aliasType, aliasName, aliasTrees, constr, extr, refTpe, params, monadParam)) =>

            val (monadTree, code, ib) = {
              val upstacked = upstack((tpeStack :+ ttpt), innerBlock)
              //vprintln(s"-----> upstacked:"+upstacked + "   --- innerBlock:"+innerBlock+ "    --- innerBlock.tpe:"+innerBlock.tpe)
              upstacked match {
                // NOT UPSTACKABLE AT ALL
                case Some(EmptyTree) =>
                  val code = q"""{ ($param) =>
                    ${splicer(innerBlock)}
                  }"""

                  (monadTMap(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(code), code, innerBlock)

                // USE TREE DIRECTLY
                case None =>
                  val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                  val code = q"""($param) =>
                    ${constr(ctpe)(splicer(innerBlock))}
                  """

                  (monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(code), code, innerBlock)

                // UPSTACKABLE
                case Some(b) =>
                  val ctpe = if(isLastUnit(innerBlock.tpe)) tq"${definitions.UnitTpe}" else tpt

                  val code = q"""($param) =>
                    ${constr(ctpe)(splicer(b))}
                  """

                  (monadTBind(arg.tpe, innerBlock.tpe, constr(tpt))(refexpr)(code), code, b)
              }

            }

            val block = api.typecheck(
              q"""{
                ..$aliasTrees
                ${extr(monadTree)}
              }"""
            )

            // VERY IMPORTANT TO RELOCATE OWNERS (this is the tricky part)
            ib.changeOwner(value.owner, code.symbol)

            block

        }

      }

      def transformSingle(tree: Tree, others: List[Tree]): List[Tree] = tree match {

        case tree@q"$mods val $value: $tpt = $fun($arg)" if(isSnoopX(fun)) =>
          val code = buildCode(
            monadStackFromSnoopX(fun, monadTpeHelpers),
            resultTpeHelper, tree.symbol, tpt, arg, others
          )

          val newValue = copyValChangeTpe(api)(tree, mods, value, code.tpe, code)
          val newValueRef = gen.mkAttributedStableRef(newValue.symbol).setType(code.tpe).setPos(tree.pos)
          List(newValue, newValueRef)

        case q"$mods val $value: $tpt = $arg" =>
          val tArg = arg match {
            case q"if ($cond) $thenp else $elsep" =>
              // TODO choose better type than tThen.tpe using tElse.tpe
              val tThen0 = api.typecheck(q"{ ..${transformSingle(thenp, List())} }")
              val tThen = api.typecheck(upstack(monadTpeHelpers, tThen0) match {
                // NOT UPSTACKABLE AT ALL
                case Some(EmptyTree) | None => tThen0
                case Some(c) => c
              })
              println("TTHEN:"+tThen.tpe)
              val tElse0 = api.typecheck(q"{ ..${transformSingle(elsep, List())} }")
              val tElse = api.typecheck(upstack(monadTpeHelpers, tElse0) match {
                // NOT UPSTACKABLE AT ALL
                case Some(EmptyTree) | None => tElse0
                case Some(c) => c
              })

              val tCond = api.recur(cond)

              forceType(api)(treeCopy.If(tree, tCond, tThen, tElse), tThen.tpe)

            case q"$sel match { case ..$cases }" =>
              val tSel = api.recur(sel)
              val tCases = cases map {
                case c@cq"$pat if $guard => $body" =>
                  val tBody = api.typecheck(q"{ ..${transformSingle(body, List())} }")
                  treeCopy.CaseDef(c, pat, guard, tBody)
              }
              treeCopy.Match(tree, tSel, tCases)

            case arg => api.recur(arg)
          }

          val newValue = copyValChangeTpe(api)(tree, mods, value, tArg.tpe, tArg)
          val newValueRef = gen.mkAttributedStableRef(newValue.symbol).setType(tArg.tpe).setPos(tree.pos)

          symMap += (tree.symbol) -> (q"$newValueRef", TpeHelper(tArg.tpe))
          val res = List(newValue) ++ transformMultiple(others)

          // adds symbol to be replaced in other branches
          res


        case Block(stats, expr) =>
          val t = transformMultiple(stats :+ expr)
          val tOthers = transformMultiple(others)
          t ++ tOthers

        /*case tree@If(cond, thenp, elsep) =>
          val tThen = listToBlock(transformSingle(thenp, List()))
          val tElse = listToBlock(transformSingle(elsep, List()))
          treeCopy.If(tree, api.recur(cond), tThen, tElse).setType(tThen.tpe) +: transformMultiple(others)*/

        case e if (symMap contains e.symbol) =>
          val (p, tpeHelper) = symMap(e.symbol)
          p match {
            //case q"$_ val $value: $_ = $_" =>
            //  treeCopy.Ident(p, value) +: transformMultiple(others)

            case tree =>
              val otherss = transformMultiple(others)
              //transformMultiple(tree +: otherss)
              tree +: otherss
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
        case List(e)  => e
        case t        => listToBlock(t)
      }
    }

    val upstacked = upstack((monadTpeHelpers :+ resultTpeHelper), transformed)

    upstacked match {
      // NOT UPSTACKABLE AT ALL
      case Some(EmptyTree) | None => transformed
      case Some(c) => c
    }
    //println("transformed:"+showRaw(transformed, printTypes = true, printKinds = true))
    //println("-----------------***------------------------")
    //transformed.foreach{ tree => println(s"-----FOREACH---> tree:$tree o:"+(if(tree != null && tree.symbol != null) tree.symbol.owner else "null")) }
    //transformed
  }


}


