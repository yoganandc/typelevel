package typelevel

object Main extends App {
  import scala.reflect.runtime.universe._
  def show[T](value: T)(implicit tag: TypeTag[T]): String =
    tag.toString.replace("typelevel.Main.", "")

  trait Nat
  class _0 extends Nat
  class Succ[A <: Nat] extends Nat

  type _1 = Succ[_0]
  type _2 = Succ[_1] // = Succ[Succ[_0]]
  type _3 = Succ[_2] // = Succ[Succ[Succ[_0]]]
  type _4 = Succ[_3] // ... and so on
  type _5 = Succ[_4]

  sealed trait <[A <: Nat, B <: Nat]
  object < {
    def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[A, B] = lt
    implicit def ltBasic[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}
    implicit def inductive[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[Succ[A], Succ[B]] = new <[Succ[A], Succ[B]] {}
  }

  sealed trait <=[A <: Nat, B <: Nat]
  object <= {
    def apply[A <: Nat, B <: Nat](implicit lte: <=[A, B]): <=[A, B] = lte
    implicit def lteBasic[B <: Nat]: <=[_0, B] = new <=[_0, B] {}
    implicit def inductive[A <: Nat, B <: Nat](implicit lt: <=[A, B]): <=[Succ[A], Succ[B]] = new <=[Succ[A], Succ[B]] {}
  }

  val validComparison1: <[_2, _3] = <[_2, _3]
  val validComparison2: <=[_3, _3] = <=[_3, _3]

  trait +[A <: Nat, B <: Nat, S <: Nat]
  object + {
    def apply[A <: Nat, B <: Nat, S <: Nat](implicit plus: +[A, B, S]): +[A, B, S] = plus

    implicit val zero: +[_0, _0, _0] = new +[_0, _0, _0] {}
    implicit def basicRight[A <: Nat](implicit lt: _0 < A): +[_0, A, A] = new +[_0, A, A] {}
    implicit def basicLeft[A <: Nat](implicit lt: _0 < A): +[A, _0, A] = new +[A, _0, A] {}
    implicit def inductive[A <: Nat, B <: Nat, S <: Nat](implicit plus: +[A, B, S]): +[Succ[A], Succ[B], Succ[Succ[S]]] =
      new +[Succ[A], Succ[B], Succ[Succ[S]]] {}
  }

  val validComparison3: +[_2, _3, _5] = +.apply

  println(show(validComparison1))
}
