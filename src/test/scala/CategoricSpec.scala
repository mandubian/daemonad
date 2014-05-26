import org.scalatest._

import scalaz._
import Scalaz._
import scala.concurrent._

import categoric.scalaz._

class DaemonadSpec extends FlatSpec with Matchers {
  /*"Daemonad" should "be monadic" in {
    val toto: Option[Int] = monadic[Option] {
      val a = Some(5)
      val b = Some(10)
      val c = Some(15)

      val a1 = snoop1(a)
      val b1 = snoop1(b)
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
        val b = Future(Some(8))
        val c = Future(Some(6))
        val a1 = snoop2(a)
        val b1 = snoop2(b)
        println(a1 + b1 + snoop2(c))
      }, duration.Duration("1 second")
    )
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

  it should """snoop2 on Future[\/[String, Int]]""" in {
    Await.result(
      monadic[Future, ({ type l[T] = \/[String, T] })#l] {
        val a = Future(\/-(5))
        val b = Future(\/-(3))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (\/-(8))
  }

  /*it should """snoop2 on Future[\/[String, Int]] with -\/""" in {
    Await.result(
      monadic[Future, ({ type l[T] = \/[String, T] })#l] {
        val a = Future(\/-(5))
        val b = Future(-\/("toto"))
        snoop2(a) + snoop2(b)
      }, duration.Duration("1 second")
    ) should equal (-\/("toto"))
  }*/

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

  it should "snoop2 for Option[Either[String, Int]]" in {
    val a = Some(\/-(2))
    val b = Some(\/-(5))
    monadic[Option, ({ type l[T] = \/[String, T] })#l] {
      snoop2(a) + snoop2(b)
    } should equal (Some(\/-(7)))
  }

  /*it should "snoop2 for Option[Either[String, Int]] typealiased" in {
    val a = Some(\/-(2))
    val b = Some(\/-(5))
    type EitherString[T] = ({ type l[T] = \/[String, T] })#l[T]
    monadic[Option, EitherString] {
      snoop2(a) + snoop2(b)
    } should equal (Some(\/-(7)))
  }*/

  /*"Daemonad" should "be multimonadic" in {
    // monadic[Option] {
    //   val a = Some(5)
    //   val b = Some(10)
    //   val c = Some(16)

    //   snoop1(a) + snoop1(b) + snoop1(c)
    // }

    // println("Coucou:"+ Await.result(
    //   monadic[Future, List] {
    //     val a = Future { List(5, 10) }
    //     val b = List(8, 16)
    //     //val c = Future { 10 }
    //     val a1 = snoop2(a)
    //     //val c1 = snoop1(c)
    //     val b1 = snoop1(b)
    //     val b2 = b1 + 1
    //     a1 + b2
    //   }, duration.Duration("1 second")
    // ))

    println("Coucou:"+ Await.result(
      monadic[Future, List, Option] {
        val a = Future ( List( Some(5), Some(10) ) )
        //val b = Future ( List( Some(6), Some(11) ) )
        val b = List(Some(8))
        val c = Some(11)
        val a1 = snoop3(a)
        val b1 = snoop2(b)
        //val c1 = snoop1(c)
        //val b1 = snoop1(b)
        //val b2 = b1 + 2
        //a1 + b2
        a1 + b1 + snoop1(c)
      }, duration.Duration("1 second")
    ))
  }*/

    /*type ReaderTOption[A, B] = ReaderT[Option, A, B]
    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }*/

    /*type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]

    object StateTReaderTOption extends StateTInstances with StateTFunctions {
      def apply[C, S, A](f: S => (S, A)) = new StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
        def apply(s: S) = f(s).point[({type l[X] = ReaderTOption[C, X]})#l]
      }
      def get[C, S]: StateTReaderTOption[C, S, S] =
        StateTReaderTOption { s => (s, s) }
      def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
        StateTReaderTOption { _ => (s, ()) }
    }

    type StateTReaderTOption[C, S, A] = ({type l[X] = StateTReaderTOption[]})
    def R: Reader[String, (String, String)] = 
      monadic[StateTReaderTOption[A, String, String] forSome { type A }] {

      }*/

    /*type TReader[T] = Reader[String, T]

    def r = monadic[TReader] {
      def myName(step: String): TReader[String] = Reader[String, String] { step + ", I am " + (_: String) }
      (snoop1(myName("toto")), snoop1(myName("tata")))
    }

    println(r("toto"))*/

    //type EitherC[T] = Either[String, T]
    //monadic[Future, EitherC]

    /*type StringReader[C] = ({ type l[C] = ReaderT[Id, C, String] })#l[C]

    object StringReader extends KleisliInstances with KleisliFunctions {

      def apply[C](f: C => String): StringReader[C] = kleisli { (c:C) => f(c).point[Id] }
    }
      implicit val M: Monad[StringReader] = implicitly[Monad[({ type l[C] = Reader[C, String] })#l]]

    def R: Reader[String, (String, String)] = monadic[StringReader] {
      def myName(step: String) = StringReader { step + ", I am " + (_: String) }

      val fst = myName("First")
      val sec: StringReader[String] = myName("Second") >=> StringReader { (_:String) + "dy" }

      (snoop1(fst), snoop1(sec))
    }

    println(R.apply("Toto"))*/

/*
  monadic[Future, Option] {
      val a = Future{ Thread.sleep(1000) ; println("5"); Some(5) }
      val b = Future{ Thread.sleep(500) ; println("8"); Some(8) }
      val a1 = snoop2(a)
      val b1 = snoop2(b)
      val b2 = b1 + 2
      println (a1 + b2)
    }

    val a1 = a flatMap { _a =>
      val a$ = a1
      val b1 = snoop2(b)
      val b2 = b1 + 2
      a$ + b2
    }

a1 -> _a

    val a1 = a flatMap { _a =>
      val a$ = snoop1(_a)
      val b1 = b flatMap { _b =>
        val b2 = b1 + 2
        a1 + b2
      }
      b1
    }

a1 -> _a | b1 => snoop1(_b)

    val a1 = a flatMap { _a =>
      val b1 = b flatMap { _b =>
        val b2 = b1 + 2
        a1 + b2
      }
      b1
    }

    val a1 = a flatMap { _a =>
      val b1 = b flatMap { _b =>
        val b2 = snoop1(_b) + 2
        snoop1(_a) + b2
      }
*/


  /*"Daemonad" should "be monadic" in {
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
              //snoop1(a)
              //snoop1(Some(snoop1(a)))
              val d = snoop1(a)
              d match {
                case 5  => snoop1(a) + snoop1(b)
                case 10 => snoop1(a) + snoop1(c)
              }
              //if(snoop1(a) == 5) snoop1(a) + snoop1(b) else snoop1(a) + snoop1(b) + snoop1(c)
              //snoop1(a) + snoop1(b) + snoop1(c)
              //if(snoop1(a) == 5) true else false
            }*/

    val toto: Toto[Int] = monadic[Toto] {
      val a = Toto(5)
      val b = Toto(10)

      snoop1(a) match {
        case 5 => snoop1(b)
        case a => a + snoop1(b)
      }
    }
    println( "!!!!!!!! RESULT:" + toto)
  }*/

  /*"Daemonad" should "be multimonadic" in {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    monadic[List, Option] {
      val a = Future(Some(5))
      val b = Future(Some(5))
      snoop2(a) + snoop2(b)
    }
  }*/

/*  "Daemonad" should "be applicative" in {
    val a = 5.successNel[Int]
    val b = 10.failureNel[Int]

    (a |@| b)( (a, b) => a + b )

    val app = applicative[ValidationNEL] {
      val a = 5.successNel[Int]
      val b = 10.failureNel[Int]
      val d = snoop1(a) + snoop1(b) + 5
      val c = 15.failureNel[Int]
      if(snoop1(a) > 2) d + snoop1(c)
      else snoop1(b)
    }
    //
    val a = 5.successNel[Int]
    val b = 10.failureNel[Int]
    val _a = a
    val __a = snoop1(a)
    val _b = b
    val __b = snoop1(b)
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