package categoric
package scalaz

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.reflect.internal.Flags

import core._
import categoric.monadic._


trait ScalazMonadicTransform extends MonadicTransform with ScalazMonadicUtils {
  import c.universe._
  import Flag._
  import c.internal._
  import decorators._

  override def snoop1MethodSymbol: Symbol = {
    c.mirror.staticPackage("categoric.scalaz").typeSignature.member("snoop1": TermName)
  }

  override def snoop2MethodSymbol: Symbol = {
    c.mirror.staticPackage("categoric.scalaz").typeSignature.member("snoop2": TermName)
  }

  override def snoop3MethodSymbol: Symbol = {
    c.mirror.staticPackage("categoric.scalaz").typeSignature.member("snoop3": TermName)
  }

  private def buildMonadTT(mtpe: Type, tname: TypeName)(tree: Tree): (Type, Tree) = {
    val monadT = c.typecheck(tree, c.TYPEmode).tpe
    val monadTClean = monadT map { _ match {
      case t@TypeRef(prefix, sym, args) if sym.asType.isExistential =>
        println("@@@@@@@@ "+typeRef(prefix, sym.owner.newTypeSymbol("_").setInfo(NoType), args))
        typeRef(prefix, sym.owner.newTypeSymbol("_").setInfo(NoType), args)
        //typeRef(NoPrefix, mtpe.typeConstructor.typeSymbol, args)
      case tpe => tpe
    }}
    val PolyType(_, monadTRaw) = monadT.typeConstructor.normalize
    println("monadTClean:"+monadTClean)
    (monadTClean, tq"${monadTRaw.typeSymbol}[${mtpe.typeSymbol}, $tname]")
  }

  private def buildMonadTypeAlias(api: TypingTransformApi, monadTTpe: Type, mtpe: TpeHelper, innerTpe: Type): (TypeName, TermName, Tree) = {
    val nm = c.fresh(monadTTpe.typeSymbol.name.decodedName.toString)
    val typeName = newTypeName(nm)
    val termName = newTermName(nm)
    //val sym = newTypeSymbol(api.currentOwner, typeName).setInfo(typeBounds(definitions.NothingTpe, definitions.AnyTpe))

    val tree = q"""{
      type $typeName[T] = ${monadTTpe.typeSymbol}[${mtpe.tpe.typeSymbol}, T]
      object $termName {
        def apply[T](t: ${mtpe.tpe.typeSymbol}[${innerTpe.typeSymbol}[T]]): $typeName[T] = ${monadTTpe.typeSymbol.companion}.apply[${mtpe.tpe.typeSymbol}, T](t)
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


  def inferMonadTransformerTpe3
        (api: TypingTransformApi, tpes: List[TpeHelper], knownAliasTpes: collection.mutable.ListBuffer[AliasTpe])
        : AliasTpe =
  {
    def step(stack: List[TpeHelper]): AliasTpe = {
      stack match {
        // M[N]
        case ntpe :: mtpe :: Nil =>
          if(ntpe.existential <:< typeOf[Option[_]]) {
            //knownAliasTpes find { atpe => atpe =:= ntpe } match {
              //case None =>
                println("knownAliasTpes NOT FOUND "+ntpe)
                val (aliasType, aliasName, aliasTree) = buildMonadTypeAlias(api, typeOf[_root_.scalaz.OptionT[M forSome { type M[_] }, _]], mtpe, typeOf[Option[_]])

                val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
                val extr = (t: Tree) => q"$t.run"

                val aliasTpe = AliasTpe(aliasType, aliasName, blockToList(gen.stabilize(aliasTree)), constr, extr, ntpe)
                knownAliasTpes += aliasTpe
                aliasTpe

              //case Some(aliasTpe) =>
                //println("knownAliasTpes FOUND "+aliasTpe)
                //aliasTpe
            //}
          } else if(ntpe.existential <:< typeOf[List[_]]) {
            //knownAliasTpes find { atpe => atpe =:= ntpe } match {
              //case None =>
                println("knownAliasTpes NOT FOUND "+ntpe)
                val (aliasType, aliasName, aliasTree) = buildMonadTypeAlias(api, typeOf[_root_.scalaz.ListT[M forSome { type M[_] }, _]], mtpe, typeOf[List[_]])
                //println("(aliasType, aliasName, aliasTree)="+(aliasType, aliasName, aliasTree))

                val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
                val extr = (t: Tree) => q"$t.underlying"

                val aliasTpe = AliasTpe(aliasType, aliasName, blockToList(aliasTree), constr, extr, ntpe)
                knownAliasTpes += aliasTpe
                aliasTpe

            //   case Some(aliasTpe) =>
            //     println("knownAliasTpes FOUND "+aliasTpe)
            //     aliasTpe
            // }
          } else if(ntpe.existential <:< typeOf[_root_.scalaz.\/[_, _]]) {
            if(ntpe.tpe.resultType.typeArgs.size > 0) {
              // knownAliasTpes find { atpe => atpe =:= ntpe } match {
              //   case None =>
                  println("knownAliasTpes NOT FOUND "+ntpe)
                  val aTpe = ntpe.tpe.resultType.typeArgs(0)

                  val eitherTTpe = c.typecheck(
                    tq"_root_.scalaz.EitherT[ M forSome { type M[_] }, ${aTpe}, _]",
                    c.TYPEmode
                  ).tpe

                  val eitherTpe = c.typecheck(
                    tq"_root_.scalaz.\/[${aTpe}, _]",
                    c.TYPEmode
                  ).tpe

                  val (aliasType, aliasName, aliasTree) = buildMonadType2Alias(api, eitherTTpe, aTpe, mtpe, eitherTpe)
                  //println("(aliasType, aliasName, aliasTree)="+(aliasType, aliasName, aliasTree))

                  val constr = (tpe: Tree) => (t: Tree) => q"""${aliasName}[$tpe]($t)"""
                  val extr = (t: Tree) => q"$t.run"
                  val aliasTpe = AliasTpe(aliasType, aliasName, blockToList(aliasTree), constr, extr, ntpe)
                  knownAliasTpes += aliasTpe
                  aliasTpe

              //   case Some(aliasTpe) =>
              //     println("knownAliasTpes FOUND "+aliasTpe)
              //     aliasTpe
              // }
            } else {
              c.abort(c.enclosingPosition, """
Can't manage Alias Types to type-lambda based on \/[A, _] for now...
Please put the type lambda directly in the monadic type parameters like:
monadic[Option, ({ type l[T] = \/[String, T] })#l]
""")
            }

          } else {
            c.abort(c.enclosingPosition, s"Can't manage this Monad Stack $stack")
          }

        // L[M[N]]
        case l@ntpe :: tail =>
          val AliasTpe(tailAliasType, tailAliasName, tailAliasTrees, tailConstr, tailExtr, refTpe) = step(tail)
          if(ntpe.existential <:< typeOf[Option[_]]) {
            // knownAliasTpes find { atpe => atpe =:= ntpe } match {
            //   case None =>
                println("knownAliasTpes NOT FOUND "+ntpe)
                val (aliasType, aliasName, aliasTrees) = buildMonadTypeAlias(api, typeOf[_root_.scalaz.OptionT[M forSome { type M[_] }, _]], tailAliasType, typeOf[Option[_]])
                //println("(aliasType, aliasName, aliasTrees)="+(aliasType, aliasName, aliasTrees))

                val constr = (tpe: Tree) => (t: Tree) => {
                  val ttpe = tq"${ntpe.tpe.typeSymbol}[$tpe]"
                  q"""${aliasName}[$tpe](${tailConstr(ttpe)(t)})"""
                }
                val extr = (t: Tree) => tailExtr(q"$t.run")

                val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe)
                knownAliasTpes += aliasTpe
                aliasTpe

            //   case Some(aliasTpe) =>
            //     println("knownAliasTpes FOUND "+aliasTpe)
            //     aliasTpe
            // }
          } else if(ntpe.existential <:< typeOf[List[_]]) {
            // knownAliasTpes find { atpe => atpe =:= ntpe } match {
            //   case None =>
                println("knownAliasTpes NOT FOUND "+ntpe)
                val (aliasType, aliasName, aliasTrees) = buildMonadTypeAlias(api, typeOf[_root_.scalaz.ListT[M forSome { type M[_] }, _]], tailAliasType, typeOf[List[_]])
                //println("(aliasType, aliasName, aliasTrees)="+(aliasType, aliasName, aliasTrees))

                val constr = (tpe: Tree) => (t: Tree) => {
                  val ttpe = tq"${ntpe.tpe.typeSymbol}[$tpe]"
                  q"""${aliasName}[$tpe](${tailConstr(ttpe)(t)})"""
                }
                val extr = (t: Tree) => tailExtr(q"$t.run")

                val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe)
                knownAliasTpes += aliasTpe
                aliasTpe

            //   case Some(aliasTpe) =>
            //     println("knownAliasTpes FOUND "+aliasTpe)
            //     aliasTpe
            // }

          } else if(ntpe.existential <:< typeOf[_root_.scalaz.\/[_, _]]) {
            if(ntpe.tpe.resultType.typeArgs.size > 0) {
              // knownAliasTpes find { atpe => atpe =:= ntpe } match {
              //   case None =>
                  println("knownAliasTpes NOT FOUND "+ntpe)
                  val aTpe = ntpe.tpe.resultType.typeArgs(0)
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

                  val aliasTpe = AliasTpe(aliasType, aliasName, tailAliasTrees ++ blockToList(aliasTrees), constr, extr, ntpe)
                  knownAliasTpes += aliasTpe
                  aliasTpe

              //   case Some(aliasTpe) =>
              //     println("knownAliasTpes FOUND "+aliasTpe)
              //     aliasTpe
              // }

            } else {
              c.abort(c.enclosingPosition, """
Can't manage Alias Types to type-lambda based on \/[A, _] for now...
Please put the type lambda directly in the monadic type parameters like:
monadic[Option, ({ type l[T] = \/[String, T] })#l]
""")
            }
          } else {
            c.abort(c.enclosingPosition, s"Can't manage this Monad Stack $stack")
          }
      }
    }

    step(tpes.reverse)
  }
}

object ScalazMonadMacro extends CategoricMacro {

  override def blockImpl[M[_], T: c0.WeakTypeTag](c0: Context)
                                                (body: c0.Expr[T])
                                                (implicit mw: c0.WeakTypeTag[M[_]]): c0.Expr[M[T]] = 
  {
    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadicTransform {
      val c: c0.type = c0
    }

    //val code = system.transform1[M, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[T])
    val code = transformer.transform(body.tree)(weakTypeTag[M[_]].tpe :: Nil, weakTypeTag[T].tpe)

    println("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

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

    //val code = transformer.transform2[M, N, T](body.tree)(weakTypeTag[M[_]], weakTypeTag[N[_]], weakTypeTag[T])
    val code = transformer.transform(body.tree)(weakTypeTag[M[_]].tpe :: weakTypeTag[N[_]].tpe :: Nil, weakTypeTag[T].tpe)

    println("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[M[N[T]]](code)
  }

  override def blockImpl3[L[_], M[_], N[_], T: c0.WeakTypeTag](c0: Context)
                                                (body: c0.Expr[T])
                                                (implicit lw: c0.WeakTypeTag[L[_]],
                                                          mw: c0.WeakTypeTag[M[_]],
                                                          nw: c0.WeakTypeTag[N[_]]): c0.Expr[L[M[N[T]]]] =
  {
    import c0.universe._, c0.internal._, decorators._

    val transformer = new ScalazMonadicTransform {
      val c: c0.type = c0
    }

    //val code = transformer.transform3(body.tree)(weakTypeTag[L[_]].tpe, weakTypeTag[M[_]].tpe, weakTypeTag[N[_]].tpe, weakTypeTag[T].tpe)
    val code = transformer.transform(body.tree)(weakTypeTag[L[_]].tpe :: weakTypeTag[M[_]].tpe :: weakTypeTag[N[_]].tpe :: Nil, weakTypeTag[T].tpe)

    println("CODE:"+code)

    //println("AST:"+showRaw(code, printTypes = true, printIds = true, printKinds = true))

    c0.Expr[L[M[N[T]]]](code)
  }
}


/*private def buildMonadTLambda(monadTTT: Tree, tname: TypeName): Tree = {
    tq"({ type l[$tname] = $monadTTT })#l"
  }

  private def buildConstrExtr(mtpe: Type)(monadTT: Type, extractorName: String): (Tree => Tree => Tree, Tree => Tree) = {
      val extractor = monadTT.member(newTermName(extractorName)).asMethod
      /*val PolyType(_, mtperaw) = mtpe.typeConstructor.normalize
      println("MTPERAW:"+mtpe)*/
      val constr = (tpe: Tree) => (t: Tree) => q"${monadTT.typeSymbol.companion}[$mtpe, $tpe]($t)"
      val extr = (t: Tree) => q"$t.$extractor"

      (constr, extr)
  }

  private def buildConstrExtr(tpes: List[Type])(monadTT: Type, extractorName: String): (Tree => Tree => Tree, Tree => Tree) = {
      val extractor = monadTT.member(newTermName(extractorName)).asMethod
      /*val PolyType(_, mtperaw) = mtpe.typeConstructor.normalize
      println("MTPERAW:"+mtpe)*/
      val constr = (tpe: Tree) => (t: Tree) => q"${monadTT.typeSymbol.companion}[({ type l[T] = Future[List[T]] })#l, $tpe]($t)"
      val extr = (t: Tree) => q"$t.$extractor"

      (constr, extr)
  }

  private def buildConstr(mtpe: Type)(monadTT: Type)(otherConstr: Tree => Tree => Tree): Tree => Tree => Tree = {
      val constr = (tpe: Tree) => (t: Tree) =>
        q"""${monadTT.typeSymbol.companion}[$mtpe, $tpe](${otherConstr(tpe)(t)})"""

      constr
  }

  private def buildExtr(mtpe: Type)(monadTT: Type, extractorName: String)(otherExtr: Tree => Tree): Tree => Tree = {
      val extractor = monadTT.member(newTermName(extractorName)).asMethod
      val extr = (t: Tree) => q"${otherExtr(t)}.$extractor"

      extr
  }*/
  /*def inferMonadTransformer[M[_], N[_], T]
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
  }*/

  /*override def inferMonadTransformerTpe
        (api: TypingTransformApi)
        (mTpe: Type, nTpe: Type, tTpe: Type)
        : (Type, Tree => Tree => Tree, Tree => Tree) =
  {

    //val wtpe = weakTypeOf[_root_.scalaz.Monad[({ type l[T] = _root_.scalaz.OptionT[M, T] })#l]]
    val wtpe = c.typecheck(tq"_root_.scalaz.Monad[({ type l[T] = _root_.scalaz.OptionT[$mTpe, T] })#l]", c.TYPEmode).tpe
    val mtpe = mTpe.typeConstructor
    val wtpe2 = wtpe.map { _ match {
      case TypeRef(prefix, sym, args) if sym.isFreeType =>
        typeRef(NoPrefix, mtpe.typeSymbol, args)
      case tpe => tpe
    }}

    println(s"wtpe:$wtpe wtpe2:$wtpe2")
    val i = c.inferImplicitValue(wtpe2, silent = false)
    println(s"i:$i")

    val tpe = c.typecheck(tq"_root_.scalaz.OptionT[$mTpe, _]", c.TYPEmode).tpe
    //val tpe = weakTypeOf[_root_.scalaz.OptionT[M, _]]
    val tpe2 = tpe.map { _ match {
      case TypeRef(prefix, sym, args) if sym.isFreeType =>
        typeRef(NoPrefix, mtpe.typeSymbol, args)
      case tpe => tpe
    }}

    val norm = tpe2.typeConstructor.normalize
    val PolyType(params, raw) = norm
    val customTpe = appliedType(raw, List(mTpe.typeConstructor, tTpe))

    val constructor = customTpe.member(nme.CONSTRUCTOR).asMethod
    val extractor = customTpe.member(newTermName("run")).asMethod

    println("const:"+constructor)
    def constr(tpe: Tree)(t: Tree) = q"${customTpe.typeSymbol.companion}[$mtpe, $tpe]($t)"
    def extr(t: Tree) = q"$t.$extractor"

    (wtpe2, constr, extr)
  }

  private def inferMonadTImplicit(mtpe: Type)(tree: Tree): Tree = {
    val monadMonadT = c.typecheck(tree, c.TYPEmode).tpe
    println(s"monadMonadT:$monadMonadT")
    /*val monadMonadTClean = monadMonadT map { _ match {
      case TypeRef(prefix, sym, args) if sym.isFreeType =>
        typeRef(NoPrefix, mtpe.typeConstructor.typeSymbol, args)
      case tpe => tpe
    }}*/
    // TODO error if no implicit
    //c.inferImplicitValue(monadMonadT, silent = false)
    tree
  }*/

/*private def buildTpe(tpes: List[Type], tname: TypeName): Type = {
    /*def step(tpes: List[Type]): Tree = {
      tpes match {
        case List() => EmptyTree
        case List(tpe) => tq"${tpe.typeSymbol}[_]"
        case head :: tail => tq"${head.typeSymbol}[${step(tail)}]"
      }
    }
    val t = step(tpes.reverse)*/
    //val extpe = c.typecheck(tq"${tpes.head.typeSymbol}[_]", c.TYPEmode).tpe

    //val ttpe = newTypeSymbol(NoSymbol, tname).toType
    //println("extpe:"+extpe)
    //val t = appliedTypes( (ttpe +: tpes).reverse )
    //val t2 = appliedType( t, List(ttpe.toType) )
    val t3 = c.typecheck(tq"({ type l[T] = Future[List[T]] })#l", c.TYPEmode).tpe
    println("BUILDTPE:"+t3)
    t3
  }*/

  /*override def inferMonadTransformerTpe2
        (api: TypingTransformApi)
        (tpes: List[Type])
        : (Type, Tree => Tree => Tree, Tree => Tree) =
  {
    def step(stack: List[Type]): (Tree, Type, Type, Tree => Tree => Tree, Tree => Tree) = {
      val ttpe = newTypeName("T")
      stack match {
        // M[N]
        case ntpe :: mtpe :: Nil =>
          if(ntpe <:< typeOf[Option[_]].typeConstructor) {
            //val impl = inferMonadTImplicit(mtpe)(tq"_root_.scalaz.Monad[({ type l[T] = _root_.scalaz.OptionT[$mtpe, T] })#l]")
            val (monadTT, monadTTT) = buildMonadTT(mtpe, ttpe)(tq"_root_.scalaz.OptionT[$mtpe, _]")
            val (constr, extr) = buildConstrExtr(mtpe)(monadTT, "run")
            val lambda = buildMonadTLambda(monadTTT, ttpe)
            val mntpe = c.typecheck(tq"$mtpe[$ntpe[_]]", c.TYPEmode).tpe

            (lambda, monadTT, mntpe, constr, extr)

          } else if(ntpe <:< typeOf[List[_]].typeConstructor) {
            //val impl = inferMonadTImplicit(mtpe)(tq"_root_.scalaz.Monad[({ type l[T] = _root_.scalaz.ListT[$mtpe, T] })#l]")
            val (monadTT, monadTTT) = buildMonadTT(mtpe, ttpe)(tq"_root_.scalaz.ListT[$mtpe, _]")
            val (constr, extr) = buildConstrExtr(mtpe)(monadTT, "underlying")
            val lambda = buildMonadTLambda(monadTTT, ttpe)
            println("TQ="+tq"${mtpe.typeSymbol}[${ntpe.typeSymbol}[_]]")
            val mntpe = c.typecheck(tq"${mtpe.typeSymbol}[${ntpe.typeSymbol}[_]]", c.TYPEmode).tpe
            (lambda, monadTT, mntpe, constr, extr)

          } else {
            c.abort(c.enclosingPosition, s"Can't manage this Monad Stack $stack")
          }

        // L[M[N]]
        case l@ntpe :: tail =>
          if(ntpe <:< typeOf[Option[_]].typeConstructor) {
            val (tailLambda, tailTpe, tailMnTpe, tailConstr, tailExtr) = step(tail)

            //val tq"($anyref{ type $l[$tpe] = $ltpe })#$l2" = tailLambda
            //val theTpe = tq"_root_.scalaz.OptionT[$tailLambda, $ttpe]"
            val theTpe = tq"_root_.scalaz.Monad[({ type l[$ttpe] = _root_.scalaz.OptionT[$tailLambda, $ttpe] })#l]"
            println(s"""#####
              ${c.typecheck(theTpe, c.TYPEmode).tpe}
            """
              //${c.typecheck(theTpe, c.TYPEmode).tpe}
              //tq"_root_.scalaz.Monad[({ type l[$ttpe] = _root_.scalaz.OptionT[($tailLambda), $ttpe] })#l]"
            )

            //val impl = inferMonadTImplicit(tailTpe)(theTpe)
            //val (monadTT, monadTTT) = buildMonadTT(tailTpe, ttpe)(tq"_root_.scalaz.OptionT[$tailTpe, _]")
            val (monadTT, monadTTT) = buildMonadTT(tailTpe, ttpe)(tq"_root_.scalaz.OptionT[$tailLambda, _]")
            //val constr = buildConstr(tailTpe)(monadTT)(tailConstr)
            //val extr = buildExtr(tailTpe)(monadTT, "run")(tailExtr)
            val mntpe = buildTpe(l, ttpe)
            val (constr, extr) = buildConstrExtr(l)(monadTT, "run")
            val lambda = buildMonadTLambda(monadTTT, ttpe)
            //val mntpe = c.typecheck(tq"$tailMnTpe[$ntpe[_]]", c.TYPEmode).tpe
            (lambda, monadTT, mntpe, constr, extr)
          } else {
            c.abort(c.enclosingPosition, s"Can't manage this Monad Stack $stack")
          }
      }
    }

    val (_, _, mnttpe, constr, extr) = step(tpes.reverse)
    (mnttpe, constr, extr)
  }*/