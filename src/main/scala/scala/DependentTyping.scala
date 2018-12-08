package scala

import shapeless.ops.hlist.IsHCons

object DependentTyping extends App {
  import shapeless.Generic

  // trait Generic[A] { // type parameter
  //    type Repr // type member
  //    def to(value: A): Repr
  //    def from(value: Repr): A
  //  }

  def getRepr[A](value: A)(implicit gen: Generic[A]) =
    gen.to(value)

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  getRepr(Vec(1, 2))                  // Int :: Int :: HNil
  getRepr(Rect(Vec(1, 2), Vec(1, 2))) // Vec :: Vec :: HNil

  // Dependent types: the result type depend on it's value parameter

  // trait Generic2[A, Repr]

  // Now getRepr2 is useless:
  // def getRepr2[A, R](value: A)(implicit gen: Generic2[A,R]): R =
  //   ???

  /** type parameters are useful as "inputs" and type member as "outputs" */
  // Example of Shapeless dependent types

  import shapeless.{::, HList, HNil}
  import shapeless.ops.hlist.Last

//  trait Last[L <: HList] {
//    type Out
//    def apply(in: L): Out
//  }

  val last  = Last[Int :: String :: HNil]
  val last2 = Last[Int :: Int :: Int :: HNil]
  last(23 :: "Arnau" :: HNil)
  last2(10 :: 1 :: 1995 :: HNil)

  // DIY

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] {
      type Out = O
    }
    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
      inst

    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        override type Out = B
        override def apply(value: A :: B :: Rest): Out =
          value.tail.head
      }
  }

  // Why are we using Aux[L, Out] instead of just Second[L] ???

  // scala.Predef implicit apply
  implicitly[Last[Int :: HNil]] // here Repr member type is LOST

  // Last apply using Aux technique
  Last[Int :: HNil] // Last[...]{type Out = Int}
  import shapeless.the // implicitly without losing type member information
  the[Last[Int :: HNil]]

  // Back to Second

  val second = Second[String :: Int :: Boolean :: HNil]
  //  Second[String :: HNil] // wont work as expected
  second("Arnau" :: 23 :: true :: HNil)

  /** Chaining dependent functions */
  def lastField[A, Repr <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  lastField(Rect(Vec(5, 1), Vec(5, 1)))

  // Won't find any instance of gen.
  def getWrappedValueWRONG[A, H](input: A)(
      implicit
      gen: Generic.Aux[A, H :: HNil]
  ): H = gen.to(input).head

  case class Wrapper(value: Int)

  // getWrappedValue(Wrapper)(Generic[Wrapper]) // compiler gives a hint about the problem: H shouldn't appear
  // The problem is that the gen parameter is over-constrained: the compiler can't find a Repr and ensure
  // its length at the same time.
  // Solution:
  //   1. find a Generic with a suitable Repr for A;
  //   2. provide that the REpr has a head type H;

  def getWrappedValue[A, Repr <: HList, Head, Tail <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
//    ev: (Head :: Tail) =:= Repr // same as isHCons
      isHCons: IsHCons.Aux[Repr, Head, Tail]
  ): Head = gen.to(input).head

  getWrappedValue(Wrapper(42))
}
