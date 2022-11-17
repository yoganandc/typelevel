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

  trait Add[A <: Nat, B <: Nat] { type Result <: Nat }
  object Add {
    type Plus[A <: Nat, B <: Nat, S <: Nat] = Add[A, B] { type Result = S }
    // 0 + 0 = 0
    implicit val zero: Plus[_0, _0, _0] = new Add[_0, _0] { type Result = _0 }
    // for every A <: Nat st A > 0, we have A + 0 = A and 0 + A = A
    implicit def basicRight[A <: Nat](implicit lt: _0 < A): Plus[_0, A, A] = new Add[_0, A] { type Result = A }
    implicit def basicLeft[A <: Nat](implicit lt: _0 < A): Plus[A, _0, A] = new Add[A, _0] { type Result = A }
    // if A + B = S, then Succ[A] + Succ[B] = Succ[Succ[S]]
    implicit def inductive[A <: Nat, B <: Nat, S <: Nat](implicit plus: Plus[A, B, S]): Plus[Succ[A], Succ[B], Succ[Succ[S]]] =
      new Add[Succ[A], Succ[B]] { type Result = Succ[Succ[S]] }
    def apply[A <: Nat, B <: Nat](implicit plus: Add[A, B]): Plus[A, B, plus.Result] = plus
  }

  println(show(Add[_2, _3]))

  trait HList
  class HNil extends HList
  class ::[H <: Nat, T <: HList] extends HList

  trait Split[HL <: HList, L <: HList, R <: HList]
  object Split {
    implicit val basic: Split[HNil, HNil, HNil] = new Split[HNil, HNil, HNil] {}

    implicit def basic2[N <: Nat]: Split[N :: HNil, N :: HNil, HNil] =
      new Split[N :: HNil, N :: HNil, HNil] {}

    implicit def inductive[H <: Nat, HH <: Nat, T <: HList, L <: HList, R <: HList](implicit
      split: Split[T, L, R]
    ): Split[H :: HH :: T, H :: L, HH :: R] = new Split[H :: HH :: T, H :: L, HH :: R] {}

    def apply[HL <: HList, L <: HList, R <: HList](implicit split: Split[HL, L, R]): Split[HL, L, R] = split
  }

  val validSplit: Split[_1 :: _2 :: _3 :: HNil, _1 :: _3 :: HNil, _2 :: HNil] =
    Split.apply

  trait Merge[LA <: HList, LB <: HList, L <: HList]
  object Merge {
    implicit def basicLeft[L <: HList]: Merge[HNil, L, L] =
      new Merge[HNil, L, L] {}
    implicit def basicRight[L <: HList]: Merge[L, HNil, L] =
      new Merge[L, HNil, L] {}

    /*
      L1 = N1 :: T1
      L2 = N2 :: T2
      if N1 <= N2 => N1 :: {...}
      if N1 > N2  => N2 :: {...}
    */
    implicit def inductiveLTE[HA <: Nat, TA <: HList, HB <: Nat, TB <: HList, O <: HList]
    (implicit merged: Merge[TA, HB :: TB, O], lte: HA <= HB)
    : Merge[HA :: TA, HB :: TB, HA :: O]
    = new Merge[HA :: TA, HB :: TB, HA :: O] {}

    implicit def inductiveGT[HA <: Nat, TA <: HList, HB <: Nat, TB <: HList, O <: HList]
    (implicit merged: Merge[HA :: TA, TB, O], g: HB < HA)
    : Merge[HA :: TA, HB :: TB, HB :: O]
    = new Merge[HA :: TA, HB :: TB, HB :: O] {}

    def apply[LA <: HList, LB <: HList, O <: HList](implicit merge: Merge[LA, LB, O]): Merge[LA, LB, O] = merge
  }
  val validMerge: Merge[_1 :: _3 :: HNil, _2 :: HNil, _1 :: _2 :: _3 :: HNil] =
    Merge.apply

  trait Sort[L <: HList, O <: HList]
  object Sort {
    implicit val basicNil: Sort[HNil, HNil] = new Sort[HNil, HNil] {}
    implicit def basicOne[H <: Nat]: Sort[H :: HNil, H :: HNil] = new Sort[H :: HNil, H :: HNil] {}
    implicit def inductive[I <: HList, L <: HList, R <: HList, SL <: HList, SR <: HList, O <: HList]
    (implicit
       split: Split[I, L, R],
       sl: Sort[L, SL],
       sr: Sort[R, SR],
       merged: Merge[SL, SR, O]): Sort[I, O] = new Sort[I, O] {}

    def apply[L <: HList, O <: HList](implicit sorted: Sort[L, O]): Sort[L, O] = sorted
  }

  val validSort: Sort[
    _4 :: _3 :: _5 :: _1 :: _2 :: HNil,
    _1 :: _2 :: _3 :: _4 :: _5 :: HNil
  ] = Sort.apply
}
