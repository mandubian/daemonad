/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad
package scalaz

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.reflect.internal.Flags

import core._
import monad._


trait ScalazMonadTransform extends MonadTransform with ScalazMonadUtils with ScalazMonadAnalysis {
  import c.universe._
  import Flag._
  import c.internal._
  import decorators._

  override def snoop1MethodSymbol: Symbol = {
    c.mirror.staticPackage("daemonad.monad.scalaz").typeSignature.member("snoop1": TermName)
  }

  override def snoop2MethodSymbol: Symbol = {
    c.mirror.staticPackage("daemonad.monad.scalaz").typeSignature.member("snoop2": TermName)
  }

  override def snoop3MethodSymbol: Symbol = {
    c.mirror.staticPackage("daemonad.monad.scalaz").typeSignature.member("snoop3": TermName)
  }

  override def snoop4MethodSymbol: Symbol = {
    c.mirror.staticPackage("daemonad.monad.scalaz").typeSignature.member("snoop4": TermName)
  }

  private def buildMonadTT(mtpe: Type, tname: TypeName)(tree: Tree): (Type, Tree) = {
    val monadT = c.typecheck(tree, c.TYPEmode).tpe
    val monadTClean = monadT map { _ match {
      case t@TypeRef(prefix, sym, args) if sym.asType.isExistential =>
        typeRef(prefix, sym.owner.newTypeSymbol("_").setInfo(NoType), args)
        //typeRef(NoPrefix, mtpe.typeConstructor.typeSymbol, args)
      case tpe => tpe
    }}
    val PolyType(_, monadTRaw) = monadT.typeConstructor.normalize
    (monadTClean, tq"${monadTRaw.typeSymbol}[${mtpe.typeSymbol}, $tname]")
  }

  private def buildMonadTypeAlias(api: TypingTransformApi, monadTTpe: Type, mtpe: TpeHelper, innerTpe: Type): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)
    //val sym = newTypeSymbol(api.currentOwner, typeName).setInfo(typeBounds(definitions.NothingTpe, definitions.AnyTpe))

    val tree = q"""{
      type $typeName[T] = ${monadTTpe.typeSymbol}[${mtpe.tpe}, T]
      object $termName {
        def apply[T](t: ${mtpe.tpe.typeSymbol}[${innerTpe.typeSymbol}[T]]): $typeName[T] = 
          ${monadTTpe.typeSymbol.companion}.apply[${mtpe.tpe}, T](t)
      }
    }"""

    (typeName, termName, tree)
  }

  private def buildTypeAlias(api: TypingTransformApi, monadTTpe: Type, mtpe: TpeHelper, innerTpe: Type, aliasTpe: AliasTpe): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)

    val tree = q"""{
      type $typeName[T] = ${aliasTpe.typeName}[T]
      object $termName {
        def apply[T](t: ${mtpe.tpe.typeSymbol}[${innerTpe.typeSymbol}[T]]): $typeName[T] = ${aliasTpe.companionName}.apply[${mtpe.tpe.typeSymbol}, T](t)
      }
    }"""

    (typeName, termName, tree)
  }

  private def buildMonadTypeAlias(api: TypingTransformApi, monadTTpe: Type, mtpe: TypeName, innerTpe: Type): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)
    //val sym = newTypeSymbol(api.currentOwner, typeName).setInfo(typeBounds(definitions.NothingTpe, definitions.AnyTpe))

    val tree = q"""{
      type $typeName[T] = ${monadTTpe.typeSymbol}[$mtpe, T]
      object $termName {
        def apply[T](t: $mtpe[${innerTpe.typeSymbol}[T]]): $typeName[T] = ${monadTTpe.typeSymbol.companion}.apply[$mtpe, T](t)
      }
    }"""

    (typeName, termName, tree)
  }

  private def buildMonadType2Alias(api: TypingTransformApi, monadTTpe: Type, param1Tpe: Type, mtpe: TpeHelper, innerTpe: Type): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)
    //val sym = newTypeSymbol(api.currentOwner, typeName).setInfo(typeBounds(definitions.NothingTpe, definitions.AnyTpe))

    val tree = q"""{
      type $typeName[T] = ${monadTTpe.typeSymbol}[${mtpe.tpe.typeSymbol}, ${param1Tpe.typeSymbol}, T]
      object $termName {
        def apply[T](t: ${mtpe.tpe.typeSymbol}[${innerTpe.typeSymbol}[${param1Tpe.typeSymbol}, T]]): $typeName[T] = ${monadTTpe.typeSymbol.companion}.apply[${mtpe.tpe.typeSymbol}, ${param1Tpe.typeSymbol}, T](t)
      }
    }"""

    (typeName, termName, tree)
  }

  private def buildMonadType2Alias(api: TypingTransformApi, monadTTpe: Type, param1Tpe: Type, mtpe: TypeName, innerTpe: Type): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)
    //val sym = newTypeSymbol(api.currentOwner, typeName).setInfo(typeBounds(definitions.NothingTpe, definitions.AnyTpe))

    val tree = q"""{
      type $typeName[T] = ${monadTTpe.typeSymbol}[${mtpe}, ${param1Tpe.typeSymbol}, T]
      object $termName {
        def apply[T](t: ${mtpe}[${innerTpe.typeSymbol}[${param1Tpe.typeSymbol}, T]]): $typeName[T] = ${monadTTpe.typeSymbol.companion}.apply[${mtpe}, ${param1Tpe.typeSymbol}, T](t)
      }
    }"""

    (typeName, termName, tree)
  }


  override def inferMonadTransformerTpe
                (api: TypingTransformApi, knownAliasTpes: collection.mutable.ListBuffer[AliasTpe])
                (tpes: List[TpeHelper], resultTpe: TpeHelper): AliasTpe =
  {
    def step(stack: List[TpeHelper]): AliasTpe = {
      stack match {
        // M[N]
        case ntpe :: mtpe :: Nil =>
          if(ntpe.existential <:< typeOf[Option[_]]) {

            val (aliasType, aliasName, aliasTree) =
              buildMonadTypeAlias(
                api,
                typeOf[_root_.scalaz.OptionT[M forSome { type M[_] }, _]],
                mtpe,
                typeOf[Option[_]]
              )

            val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
            val extr = (t: Tree) => q"$t.run"

            val aliasTpe = AliasTpe(aliasType, aliasName, blockToList(gen.stabilize(aliasTree)), constr, extr, ntpe, List(), resultTpe)
            knownAliasTpes += aliasTpe
            aliasTpe

          } else if(ntpe.existential <:< typeOf[List[_]]) {

            val (aliasType, aliasName, aliasTree) =
              buildMonadTypeAlias(
                api,
                typeOf[_root_.scalaz.ListT[M forSome { type M[_] }, _]],
                mtpe,
                typeOf[List[_]]
              )

            val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
            val extr = (t: Tree) => q"$t.underlying"

            val aliasTpe = AliasTpe(aliasType, aliasName, blockToList(aliasTree), constr, extr, ntpe, List(), resultTpe)
            knownAliasTpes += aliasTpe
            aliasTpe

          } else if(ntpe.existential <:< typeOf[_root_.scalaz.\/[_, _]]) {

            // gets baseType as \/
            val realTpe = ntpe.tpe.baseType(typeOf[_root_.scalaz.\/[_, _]].typeSymbol)

            // gets type A from \/[A, _] which is the "not-monadic" one
            val aTpe = realTpe.typeArgs(0)

            val eitherTTpe = c.typecheck(
              tq"_root_.scalaz.EitherT[ M forSome { type M[_] }, ${aTpe}, _]",
              c.TYPEmode
            ).tpe

            val eitherTpe = c.typecheck(
              tq"_root_.scalaz.\/[${aTpe}, _]",
              c.TYPEmode
            ).tpe

            val (aliasType, aliasName, aliasTree) = buildMonadType2Alias(api, eitherTTpe, aTpe, mtpe, eitherTpe)

            val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
            val extr = (t: Tree) => q"$t.run"
            val params = (ntpe.tpe.resultType.typeArgs collect { case t if(!t.typeSymbol.isAbstract) => TpeHelper(t) })

            AliasTpe(aliasType, aliasName, blockToList(aliasTree), constr, extr, ntpe, params, resultTpe)
            /*knownAliasTpes find { atpe => atpe =:= ntpe } match {
              case None =>
                println(s"""knownAliasTpes NOT FOUND
                  NEW params:${aliasTpe.params} - monadParam: ${aliasTpe.monadParam}
                """)
                knownAliasTpes += aliasTpe
                aliasTpe

              case Some(atpe) =>
                println(s"""knownAliasTpes FOUND
                  KNOWN params:${atpe.params} - monadParam: ${atpe.monadParam}
                  NEW   params:${aliasTpe.params} - monadParam: ${aliasTpe.monadParam}
                """)
                if(aliasTpe.sameParams(atpe) && !(aliasTpe.monadParam =:= atpe.monadParam))
                  aliasTpe.copy(monadParam = atpe.monadParam)
                else aliasTpe
            }*/

          } else {
            c.abort(c.enclosingPosition, s"""Can't manage this Monad Stack ${stack.reverse.mkString("[",",","]")}""")
          }

        // L[M[N]]
        case l@ntpe :: tail =>
          val AliasTpe(tailAliasType, tailAliasName, tailAliasTrees, tailConstr, tailExtr, refTpe, aliasParams, monadParam) = step(tail)
          if(ntpe.existential <:< typeOf[Option[_]]) {

            val (aliasType, aliasName, aliasTrees) =
              buildMonadTypeAlias(
                api,
                typeOf[_root_.scalaz.OptionT[M forSome { type M[_] }, _]],
                tailAliasType,
                typeOf[Option[_]]
              )

            val constr = (tpe: Tree) => (t: Tree) => {
              val ttpe = tq"${ntpe.tpe.typeSymbol}[$tpe]"
              q"""${aliasName}[$tpe](${tailConstr(ttpe)(t)})"""
            }
            val extr = (t: Tree) => tailExtr(q"$t.run")

            val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe, List(), resultTpe)
            knownAliasTpes += aliasTpe
            aliasTpe

          } else if(ntpe.existential <:< typeOf[List[_]]) {

            val (aliasType, aliasName, aliasTrees) =
              buildMonadTypeAlias(
                api,
                typeOf[_root_.scalaz.ListT[M forSome { type M[_] }, _]],
                tailAliasType,
                typeOf[List[_]]
              )

            val constr = (tpe: Tree) => (t: Tree) => {
              val ttpe = tq"${ntpe.tpe.typeSymbol}[$tpe]"
              q"""${aliasName}[$tpe](${tailConstr(ttpe)(t)})"""
            }
            val extr = (t: Tree) => tailExtr(q"$t.run")

            val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe, List(), resultTpe)
            knownAliasTpes += aliasTpe
            aliasTpe

          } else if(ntpe.existential <:< typeOf[_root_.scalaz.\/[_, _]]) {
            // gets baseType as \/
            val realTpe = ntpe.tpe.baseType(typeOf[_root_.scalaz.\/[_, _]].typeSymbol)

            // gets type A from \/[A, _] which is the "not-monadic" one
            val aTpe = realTpe.typeArgs(0)

            val eitherTTpe = c.typecheck(
              tq"_root_.scalaz.EitherT[ M forSome { type M[_] }, ${aTpe}, _]",
              c.TYPEmode
            ).tpe

            val eitherTpe = c.typecheck(
              tq"_root_.scalaz.\/[${aTpe}, _]",
              c.TYPEmode
            ).tpe

            val (aliasType, aliasName, aliasTrees) = buildMonadType2Alias(api, eitherTTpe, aTpe, tailAliasType, eitherTpe)

            val constr = (tpe: Tree) => (t: Tree) => {
              val ttpe = tq"${ntpe.tpe.typeSymbol}[$tpe]"
              q"""${aliasName}[$tpe](${tailConstr(ttpe)(t)})"""
            }
            val extr = (t: Tree) => tailExtr(q"$t.run")

            val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe, List(), resultTpe)
            knownAliasTpes += aliasTpe
            aliasTpe

          } else {
            c.abort(c.enclosingPosition, s"Can't manage this Monad Stack $stack")
          }
      }
    }

    step(tpes.reverse)
  }

}

