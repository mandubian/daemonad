/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
package daemonad
package monad
package scalaz

import scala.language.experimental.macros

import core._
import monad._


trait ScalazMonadAnalysis extends MonadAnalysis with MonadUpstack {

  override def reportUnsupportedMonadTpe(monadTpes: List[TpeHelper], tree: c.Tree, tpe: TpeHelper): Unit = {
    val fullTpes = allAppliedTypes(monadTpes)
    val isOk = fullTpes.foldLeft(false){ (acc, fullTpe) =>
      acc || (tpe <:< fullTpe.existential)
    }

    if(!isOk) c.abort(tree.pos, s"Is this a joke? $tpe isn't of the stack type $monadTpes")

    vprintln(s"----> Checking upstackable:${monadTpes} tree:$tree tpe:$tpe "+ upstackable(monadTpes, tree))
    if(!upstackable(monadTpes, tree)) c.abort(tree.pos, s"Hey $tree of type ${tpe} isn't stackable to ${appliedTypes(monadTpes)}")

    (tpe +: tpe.allTypeArgs) foreach { arg =>
      // HARDCODED CHECKING <<<< TO BE MODIFIED
      if(arg <:< c.typeOf[_root_.scalaz.-\/[_]]) {

        c.abort(tree.pos, s"""
You're using $arg & it's not the monadic side of \\/[A, B].
I'm not a clever macro enough for this case and require a bit of help.
Please define clearly the type of your expression as the expected \\/[A, B] and I'll be happy.
For example:
type S[T] = ({ type l[T] = \\/[String, T] })#l[T]
monadic[Future, S] {
  val a: Future[S[Int]] = Future(\\/-(5))
  // NOT val b = Future(-\\/("toto"))
  val b: Future[S[Int]] = Future(-\\/("toto"))
  snoop2(a) + snoop2(b)
}

The error happens here:
""")
      }
      else if(arg <:< c.typeOf[_root_.scala.None.type]) {

        c.abort(tree.pos, s"""
You're using $arg & it's an Option[Nothing].
I'm not a clever macro enough for this case and require a bit of help.
Please define clearly the type of your expression as the expected Option[A] and I'll be happy.
For example:
monadic[Future, Option] {
  val a = Future(Some(5))
  // NOT val b = Future(None)
  val b: Future[Option[Int]] = Future(None)
  snoop2(a) + snoop2(b)
}

The error happens here:
""")
      }
    }

  }

}
