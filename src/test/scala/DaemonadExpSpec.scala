/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz._
import Scalaz._
import scala.concurrent._

import daemonad.monad.scalaz._

class DaemonadExpSpec extends FlatSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global

  def set(level: String, value: Boolean) = System.setProperty(s"daemonad.$level", value.toString)
  val levels = Seq("trace", "debug")
  def setAll(value: Boolean) = levels.foreach(set(_, value))

  implicit def futureMonad(implicit executor: ExecutionContext): Monad[Future] = new Monad[Future]  {
    override def bind[A, B](fa: Future[A])(f: A ⇒ Future[B]) = fa flatMap f
    override def point[A](a: ⇒ A) = Future(a)
    override def map[A, B](fa: Future[A])(f: A ⇒ B) = fa map f
  }

  setAll(true)

  case class Toto[+A](a: A)

  implicit object TotoMonad extends Monad[Toto] {
    def bind[A, B](fa: Toto[A])(f: A => Toto[B]): Toto[B] = {
      f(fa.a)
    }

    def point[A](a: => A): Toto[A] = Toto(a)
  }

  it should "accept custom monad with OptionT" in {
    List(Toto(Some(5)), Toto(Some(10))) map { toto =>
      monadic[Toto, Option] {
        if(snoop2(toto) < 6) 5
        else 10
      }
    } should equal (Toto(Some(5)))
  }


  // it should "snoop2 on Future[Option[Int]] with if/then/else" in {
  //   Await.result(
  //     monadic[Future, Option] {
  //       val a = Future(Some(10))
  //       val b = Future(Some(8))
  //       val c = Future(Some(6))
  //       val a1 = snoop2(a)
  //       val b1 = snoop2(b)
  //       if(a1 < snoop2(c)) b1 + snoop2(a)
  //       else snoop2(a)
  //     }, duration.Duration("1 second")
  //   ) should equal (Some(10))
  // }


  // "Daemonad" should "accept case/match" in {
  //   Await.result(
  //     monadic[Future, List, Option] {
  //       val a = Future ( List( Some(5), Some(15) ) )
  //       val b = List ( Some("youhou") )
  //       snoop3(a) match {
  //         case t if (t < 10) => snoop2(b) + t
  //         case t => "yaha" + t
  //       }
  //     }, duration.Duration("1 second")
  //   ) should equal (List(Some("youhou5"), Some("yaha15")))
  // }

}


