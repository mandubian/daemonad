import org.scalatest._

import scalaz._
import Scalaz._
import scala.concurrent._

import categoric.scalaz._

class CategoricSpec extends FlatSpec with Matchers {
  /*"Categoric" should "be monadic" in {
    val toto: Option[Int] = monadic[Option] {
      val a = Some(5)
      val b = Some(10)
      val c = Some(15)

      val a1 = peek(a)
      val b1 = peek(b)
      a1 + b1
    }
    println("toto:"+toto)
  }*/

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit def futureMonad(implicit executor: ExecutionContext): Monad[Future] = new Monad[Future]  {
    override def bind[A, B](fa: Future[A])(f: A ⇒ Future[B]) = fa flatMap f
    override def point[A](a: ⇒ A) = Future(a)
    override def map[A, B](fa: Future[A])(f: A ⇒ B) = fa map f
  }

  "Categoric" should "be multimonadic" in {

    monadic[Future, Option] {
      val a = Future{ Some(5) }
      val b = Some(8)
      val a1 = peek2(a)
      val b1 = peek(b)
      val b2 = b1 + 2
      println (a1 + b2)
    }
  }
/*
  monadic[Future, Option] {
      val a = Future{ Thread.sleep(1000) ; println("5"); Some(5) }
      val b = Future{ Thread.sleep(500) ; println("8"); Some(8) }
      val a1 = peek2(a)
      val b1 = peek2(b)
      val b2 = b1 + 2
      println (a1 + b2)
    }

    val a1 = a flatMap { _a =>
      val a$ = a1
      val b1 = peek2(b)
      val b2 = b1 + 2
      a$ + b2
    }

a1 -> _a

    val a1 = a flatMap { _a =>
      val a$ = peek(_a)
      val b1 = b flatMap { _b =>
        val b2 = b1 + 2
        a1 + b2
      }
      b1
    }

a1 -> _a | b1 => peek(_b)

    val a1 = a flatMap { _a =>
      val b1 = b flatMap { _b =>
        val b2 = b1 + 2
        a1 + b2
      }
      b1
    }

    val a1 = a flatMap { _a =>
      val b1 = b flatMap { _b =>
        val b2 = peek(_b) + 2
        peek(_a) + b2
      }
*/


  /*"Categoric" should "be monadic" in {
    case class Toto[I](a: I)

    implicit object TotoMonad extends Monad[Toto] {
      def bind[A, B](fa: Toto[A])(f: A => Toto[B]): Toto[B] = {
        f(fa.a)
      }

      def point[A](a: => A): Toto[A] = Toto(a)
    }

    /*val toto: Option[Int] = monadic[Option] {
              val a = Some(5)
              val b = Some(10)
              val c = Some(15)

              //val a = Toto(5)
              //peek(a)
              //peek(Some(peek(a)))
              val d = peek(a)
              d match {
                case 5  => peek(a) + peek(b)
                case 10 => peek(a) + peek(c)
              }
              //if(peek(a) == 5) peek(a) + peek(b) else peek(a) + peek(b) + peek(c)
              //peek(a) + peek(b) + peek(c)
              //if(peek(a) == 5) true else false
            }*/

    val toto: Toto[Int] = monadic[Toto] {
      val a = Toto(5)
      val b = Toto(10)

      peek(a) match {
        case 5 => peek(b)
        case a => a + peek(b)
      }
    }
    println( "!!!!!!!! RESULT:" + toto)
  }*/

  /*"Categoric" should "be multimonadic" in {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    monadic[List, Option] {
      val a = Future(Some(5))
      val b = Future(Some(5))
      peek2(a) + peek2(b)
    }
  }*/

/*  "Categoric" should "be applicative" in {
    val a = 5.successNel[Int]
    val b = 10.failureNel[Int]

    (a |@| b)( (a, b) => a + b )

    val app = applicative[ValidationNEL] {
      val a = 5.successNel[Int]
      val b = 10.failureNel[Int]
      val d = peek(a) + peek(b) + 5
      val c = 15.failureNel[Int]
      if(peek(a) > 2) d + peek(c)
      else peek(b)
    }
    //
    val a = 5.successNel[Int]
    val b = 10.failureNel[Int]
    val _a = a
    val __a = peek(a)
    val _b = b
    val __b = peek(b)
    val d = __a + __b + 5

    (a |@| b)( (a$, b$) => a$ + b$ + 5 )
    

    val b = 10.failureNel[Int]
    ap2(a, b)((a, b) => a+b)
  }*/
}

/*
val a = ...
val b = ...
val c = ...

val a$ = a
a$ flatMap { _a =>
  val b$ = ...
  b$ flatMap { _b =>
    _a + _b
  }
} flatMap { _ab =>
  val c$ = c
  c$ flatMap { _c =>
    _ab + _c
  }
}
*/