# Summon Daemonad & snoop into monad stacks with Scala macros

`Daemonad` is a nasty Scala macro that aims at:

- marking where you manipulate monads or stacks of monads
- compile-checking monadic behavior & implicit monad instances
- allowing to `snoop` monad values deep into (some) monad stacks in the same way as `ScalaAsync` i.e. in a pseudo-imperative way.

> This project is NOT yet stable, NOT very robust so use it at your own risks.

<br/>
## Here is what it does

Finally, here is what you can write right now.

```scala
Await.result(
  monadic[Future, Option] {
    val a = Future ( Some(9) )
    val b = Some(7)
    val c = 10
    if(snoop2(a) < 10) snoop1(b) + 10
    else c
  }, duration.Duration("1 second")
) should equal (Some(17))
```

<br/>
## Motivations

### 1 - Experiment writing a very ugly Scala macro

I wanted to write a huge & complex Scala macro to know the difficulties that it implies.

> Result: I was quite insane and I will certainly write a post-mortem article about it to show the horrible difficulties I've encountered.


### 2 - Investigate ScalaAsync generalization to all monads + (some) monad stacks

I had investigated [ScalaAsync](https://github.com/scala/async) code and thought it would be possible to generalize it to all kinds of monads and go further by managing monad stacks.

> Result : Simple monads are easy to manage (as seen also in [scala-workflow](https://github.com/aztek/scala-workflow) which I discovered very recently) and some monad stacks can be managed with Scalaz monad transformers.
> But don't think you can use all kinds of monad transformers: the limits of Scala compiler with type-lambdas in macros and my very own limits blocked me from going as far as I expected.
> So for now, it can manage Future/Option/List stacks & also \/ using type aliases.

<br/>
### 3 - Explicitly Mark monadic blocks

There are 2 ways of seeing monads:

#### You don't need or you don't want to know what is a monad...

**... And yet you use it everyday/everywhere.**

This is what most of us do using those cool `map/flatMap` functions provided by Scala libraries that allow to access the values inside `Future`, `List`, `Option` in a _protected_ way etc...
That's enough for you need in your everyday life, right?


#### You want to know or you know what is a monad ...

**... and you want to use them on purpose.**

This is what _hippy developers_ do in advanced Scala using Scalaz or even crazier in pure FP languages like Haskell.

> Guess what I prefer?

Here is the kind of code I'd like to write :

```scala
// I write my datastructure without any map/flatMap function
case class Toto[A](a: A)

// Hey I proved Toto was a monad (yes believe me)

// Let's bring this concept into my scope
implicit object TotoMonad extends Monad[Toto] {
  def bind[A, B](fa: Toto[A])(f: A => Toto[B]): Toto[B] = {
    f(fa.a)
  }

  def point[A](a: => A): Toto[A] = Toto(a)
}

...
// I create my toto
val toto = Toto("this is toto")

...

// Suddenly I decide that I must use this monadic behavior of toto
monadic[Toto] {
  val a = <snoop_value_inside_monad>(toto) // outside the monadic block, you shall not do that
  do_something_with_value(a)
  // The compiler takes care that my structure is used in a pure monadic way
  // and returns a monad Toto of the right type
}
...
```

> Ok I speak about pure functional programming and then about snooping the value out of the monad. This might seem a bit useless or even stupid compared to using directly Monad facilities. I agree and I still wonder about the sanity of this project but I'm stubborn and I try to finish what I start ;)


<br/>
## Back to code Sample

```scala
Await.result(
  monadic[Future, Option] {
    val a = Future ( Some(9) )
    val b = Some(7)
    val c = 10
    if(snoop2(a) < 10) snoop1(b) + 10
    else c
  }, duration.Duration("1 second")
) should equal (Some(17))
```

- `monadic` marks the monadic block
- `monadic[Future, List, Option]` declares that you manipulate a stack `Future[List[Option]]` (and no other)
- `snoopX` means that you want to snoop the monad value at X-th level (1, 2, 3, 4 and no more for now)
- checks for implicit instances of monads (here `List`, `Option`, `Future`) and monad transformers (here `OptionT` & `ListT`) for this stack
- translating this code into embedded `Monad.bind/point/lift/run`...
- `snoop2` is used in first position: if you have used `snoop1`, the macro would have rejected your monadic block. It's logical, when you use `flatMap`, you always start with the deeper stack of monad and I chose not to change the order of your code as I find this macro is already far too intrusive :)

I'm sure you don't want to see the code you would have to write for this, this is quite long and boring.

Let just say that this code is generated by a Scala macro for you.


> The current generated code isn't optimized at all and quite redundant but this is for next iterations.

## What is working ?

- stacks with List/Option/Either up to depth 4
- custom Monads
- a few preliminary checkings that prevent you from writing stupid code but not so much
- if/then/else & case/match in some cases


## What isn't working ?

- monadic block assigned to a val not explicitly typed.
- many things or edge-cases with mixed monad depth and if/match.
- can't use advanced monad transformers like StateT or WriterT in monadic block because Scala compiler doesn't allow what I expected with type lambdas. _This needs to be studied further._

## A very stupid example to finish with 4-depth stack

```scala
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
```

Note that:

- you have to use a type alias to Scalaz \/ to one parametric type.
- you have to help a bit the compiler about type alias S or it will infer \/[A, B] which is not what we want for the monad. This might seem tedious but I'll study if I can go around this.
- look at the result: you have a 2 elements list and at the end, you have a 8 elements list: WHAT???? No it's normal, this is the result `flatmap` between first and second and third list. 2^2^2 = 8... nothing strange but it can be surprising at first glance ;)

## TODO

- refactor all code because it's ugly, not robust and redundant!!!
- rely on `MonadTrans[F[_], _]` instead of hardcoding monad transformers as now.
- accept custom `MonadTrans` provided in the user code.
- steal some inspiration from [scala-workflow](https://github.com/aztek/scala-workflow) because I find this code cool.

## Special Thanks

- Eugene Burmako for helping me each time I was lost in macros
- Jason Zaugg for Scala Async and splicer
- Daniel James for the `snoop` name
- Thibaut Duplessis for the monad stack idea
