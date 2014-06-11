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


  it should """snoop4 on stupid stack""" in {
    type S[T] = ({ type l[T] = \/[String, T] })#l[T]
    Await.result(
      monadic[Future, S, List, Option] {
        val a: Future[S[List[Option[Int]]]] = Future(\/-(List(Some(5), Some(10))))
        val b: S[List[Option[Int]]] = \/-(List(Some(1), Some(2)))
        val c: List[Option[Int]] = List(Some(3), Some(4))
        val d: Option[Int] = Some(2)
        (snoop4(a) + snoop3(b) * 2 - snoop2(c)) / snoop1(d)
      }, duration.Duration("1 second")
    ) should equal (\/-(List(Some(2), Some(1), Some(3), Some(2), Some(4), Some(4), Some(5), Some(5))))
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


