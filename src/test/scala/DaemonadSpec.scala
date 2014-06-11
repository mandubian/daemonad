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

class DaemonadSpec extends FlatSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global

  def set(level: String, value: Boolean) = System.setProperty(s"daemonad.$level", value.toString)
  val levels = Seq("trace", "debug")
  def setAll(value: Boolean) = levels.foreach(set(_, value))

  implicit def futureMonad(implicit executor: ExecutionContext): Monad[Future] = new Monad[Future]  {
    override def bind[A, B](fa: Future[A])(f: A ⇒ Future[B]) = fa flatMap f
    override def point[A](a: ⇒ A) = Future(a)
    override def map[A, B](fa: Future[A])(f: A ⇒ B) = fa map f
  }

  setAll(false)

  "Daemonad" should "snoop1 on Future[Int]" in {
    Await.result(
      monadic[Future] {
        val a = Future(10)
        val b = Future(8)
        val c = Future(6)
        val a1 = snoop1(a)
        val b1 = snoop1(b)
        a1 + b1 + snoop1(c)
      }, duration.Duration("1 second")
    ) should equal (24)
  }

  it should "snoop1 on Future[Unit]" in {
    Await.result(
      monadic[Future] {
        val a = Future(10)
        val b = Future(8)
        val c = Future(6)
        val a1 = snoop1(a)
        val b1 = snoop1(b)
        println(a1 + b1 + snoop1(c))
      }, duration.Duration("1 second")
    )
  }

  it should "snoop1 on Future[Int] with if/then/else" in {
    def doit(t: Future[Int]): Future[Int] =
      monadic[Future] {
        val a = 10
        val b = Future(8)
        val c = Future(6)

        if(snoop1(t) == a) snoop1(b) + 6
        else if (snoop1(t) < snoop1(b)) snoop1(c)
        else snoop1(b) + 7
      }

    Await.result(
      doit(Future(10)),
      duration.Duration("1 second")
    ) should equal (14)


    Await.result(
      doit(Future(7)),
      duration.Duration("1 second")
    ) should equal (6)

    Await.result(
      doit(Future(8)),
      duration.Duration("1 second")
    ) should equal (15)
  }

  it should "snoop2 on Future[Option[Int]]" in {
    Await.result(
      monadic[Future, Option] {
        val a = Future(Some(10))
        val b = Future(Some(8))
        val c = Future(Some(6))
        val a1 = snoop2(a)
        val b1 = snoop2(b)
        a1 + b1 + snoop2(c)
      }, duration.Duration("1 second")
    ) should equal (Some(24))
  }

  it should "snoop2 on Future[Option[Unit]]" in {
    Await.result(
      monadic[Future, Option] {
        val a = Future(Some(10))
        //val b = Future(Some(8))
        //val c = Future(Some(6))
        val a1 = snoop2(a)
        //val b1 = snoop2(b)
        //println(a1 + b1 + snoop2(c))
        println(a1)
      }, duration.Duration("1 second")
    )
  }


  it should "snoop2 on Future[Option[Int]] with if/then/else" in {
    Await.result(
      monadic[Future, Option] {
        val a = Future(Some(10))
        val b = Future(Some(8))
        val c = Future(Some(6))
        val a1 = snoop2(a)
        val b1 = snoop2(b)
        if(a1 < snoop2(c)) b1 + snoop2(a)
        else snoop2(a)
      }, duration.Duration("1 second")
    ) should equal (Some(10))
  }

  it should "mix snoop2/snoop1 on Future[Option[Int]]" in {
    Await.result(
      monadic[Future, Option] {
        val a = Future(Some(10))
        val b = Some(8)
        val c = 6
        snoop2(a) + snoop1(b) + c
      }, duration.Duration("1 second")
    ) should equal (Some(24))
  }

  it should "snoop2 on Future[List[Int]]" in {
    Await.result(
      monadic[Future, List] {
        val a = Future(List(1, 2))
        val b = Future(List(3))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (List(4, 5))
  }

  it should """snoop2 on Future[\/[String, Int]] using type lambda""" in {
    Await.result(
      monadic[Future, ({ type l[T] = \/[String, T] })#l] {
        val a = Future(\/-(5))
        val b = Future(\/-(3))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (\/-(8))
  }

  it should """snoop2 on Future[\/[String, Int]] with \/- & -\/ using type alias""" in {
    type S[T] = ({ type l[T] = \/[String, T] })#l[T]
    Await.result(
      monadic[Future, S] {
        val a: Future[S[Int]] = Future(\/-(5))
        val b: Future[S[Int]] = Future(-\/("toto"))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (-\/("toto"))
  }

  it should """snoop2 on Future[\/[String, Int]] with only -\/""" in {
    type S[T] = ({ type l[T] = \/[String, T] })#l[T]
    Await.result(
      monadic[Future, S] {
        val a: Future[S[Int]] = Future(-\/("tata"))
        val b: Future[S[Int]] = Future(-\/("toto"))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (-\/("tata"))
  }

  it should "snoop3 on Future[List[Option[Int]]]" in {
    Await.result(
      monadic[Future, List, Option] {
        val a = Future ( List( Some(5), Some(9) ) )
        val b = Future ( List( Some(7), Some(12) ) )
        val a1 = snoop3(a)
        a1 + snoop3(b)
      }, duration.Duration("1 second")
    ) should equal (List(Some(12), Some(17), Some(16), Some(21)))
  }

  it should "snoop3 on Future[List[Option[Unit]]]" in {
    Await.result(
      monadic[Future, List, Option] {
        val a = Future ( List( Some(5), Some(9) ) )
        val b = Future ( List( Some(7), Some(12) ) )
        val a1 = snoop3(a)
        println(a1 + snoop3(b))
      }, duration.Duration("1 second")
    )
  }

  it should "mix snoop3/snoop2/snoop1 on Future[List[Option[Int]]]" in {
    Await.result(
      monadic[Future, List, Option] {
        val a = Future ( List( Some(5), Some(9) ) )
        val b = List( Some(7) )
        val c = Some(10)
        val a1 = snoop3(a)
        a1 + snoop2(b) + snoop1(c)
      }, duration.Duration("1 second")
    ) should equal (List(Some(22), Some(26)))
  }

  it should "accept snoop3 lazy val on Future[List[Option[Int]]]" in {
    lazy val t = monadic[Future, List, Option] {
      snoop3(Future(List(Some(5), Some(9))))
    }

    Await.result(
      t, duration.Duration("1 second")
    ) should equal (List(Some(5), Some(9)))
  }

  it should "accept snoop3 typed val on Future[List[Option[Int]]]" in {
    val t: Future[List[Option[Int]]] = monadic[Future, List, Option] {
      snoop3(Future(List(Some(5), Some(9))))
    }

    Await.result(
      t, duration.Duration("1 second")
    ) should equal (List(Some(5), Some(9)))
  }

  case class Toto[+A](a: A)

  implicit object TotoMonad extends Monad[Toto] {
    def bind[A, B](fa: Toto[A])(f: A => Toto[B]): Toto[B] = {
      f(fa.a)
    }

    def point[A](a: => A): Toto[A] = Toto(a)
  }

  it should "accept simple custom monad" in {
    val b = Toto(10)
    monadic[Toto] {
      snoop1(Toto(6)) match {
        case 5 => snoop1(b)
        case a => a + snoop1(b)
      }
    } should equal (Toto(16))

    monadic[Toto] {
      snoop1(Toto(5)) match {
        case 5 => snoop1(b)
        case a => a + snoop1(b)
      }
    } should equal (Toto(10))
  }

  it should "accept double custom monad" in {
    val a = Toto(Option(5))
    val b = Some(10)
    val c: Option[Int] = None
    monadic[Toto, Option] {
      snoop2(a) + snoop1(b)
    } should equal (Toto(Some(15)))

    monadic[Toto, Option] {
      snoop2(a) + snoop1(c)
    } should equal (Toto(None))
  }

  it should "accept custom monad with OptionT" in {
    List(Toto(Some(5)), Toto(Some(10))) map { toto =>
      monadic[Toto, Option] {
        if(snoop2(toto) < 6) 5
        else 10
      }
    } should equal (List(Toto(Some(5)), Toto(Some(10))))
  }

  import daemonad.TestUtils._

  it should "reject bad None usage" in {
    expectError("You're using None.type") {
    """
      | import _root_.scalaz._
      | import Scalaz._
      | import _root_.daemonad.monad.scalaz._
      | monadic[Option] { snoop1(None) }
    """.stripMargin
    }

    expectFatalError() {
    """
      | import _root_.scalaz._
      | import Scalaz._
      | import _root_.daemonad.monad.scalaz._
      | val a = List(None)
      | monadic[List, Option] { snoop2(a) }
    """.stripMargin
    }
  }

  it should "accept ReaderT type lambda as a 1-level monad" in {
    type ReaderTOption[A, B] = ReaderT[Option, A, B]
    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }

    type MyReader[T] = ({ type l[T] = ReaderTOption[String, T] })#l[T]

    def read(step: String): MyReader[String] = ReaderTOption { (s: String) => Some(s + step) }

    monadic[MyReader] {
      (snoop1(read("1")), snoop1(read("2")))
    }("hello") should equal (Some("hello1", "hello2"))

  }

  it should "accept int in if/then/else" in {
    Await.result(
      monadic[Future, List, Option] {
        val a = Future ( List( Some(5), Some(9) ) )
        if(snoop3(a) < 6) 5
        else 10
      }, duration.Duration("1 second")
    ) should equal (List(Some(5), Some(10)))
  }

  it should "accept returned int" in {
    Await.result(
      monadic[Future, List, Option] {
        10
      }, duration.Duration("1 second")
    ) should equal (List(Some(10)))
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
  /*it should """snoop2 on \/[String, Option[Int]]""" in {
    type S[T] = ({ type l[T] = \/[String, T] })#l[T]
    Await.result(
      monadic[S, Option] {
        val a: S[Option[Int]] = \/-(Some(5))
        val b: S[Option[Int]] = \/-(None)
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (-\/("tata"))
  }


  it should "accept ReaderT as a monad" in {
    type MyReader[T] = ({ type l[T] = Reader[String, T]})#l[T]

    monadic[MyReader, Option]{
      def rect(suffix: String): MyReader[Option[String]] = Reader{ (s:String) => Some(s + suffix) }
      val t = rect("hello")
      snoop2(t)
    }
  }*/
}


