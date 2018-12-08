import CsvEncoder._
import shapeless.{::, HList, HNil, Lazy}

object DerivingCoproduct extends App {
  import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

  implicit val doubleEncoder: CsvEnconder[Double] =
    instance(d => List(d.toString))

  implicit val hnilEncoder: CsvEnconder[HNil] =
    instance { _ =>
      Nil
    }

  implicit def hlistEncoder[H, T <: HList](implicit
                                           hEncoder: Lazy[CsvEnconder[H]],
                                           tEncoder: CsvEnconder[T]): CsvEnconder[H :: T] =
    instance {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit val cnilEncoder: CsvEnconder[CNil] =
    instance { _ =>
      throw new RuntimeException("Inconceivable")
    }

  implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                   // hnilEncoder and hlistEncoder are required to obtain a CsvEncoder[H]
                                                   hEncoder: Lazy[CsvEnconder[H]],
                                                   tEncoder: CsvEnconder[T]): CsvEnconder[H :+: T] =
    instance {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R],
      enc: Lazy[CsvEnconder[R]]
  ): CsvEnconder[A] =
    instance(a => enc.value.encode(gen.to(a)))

  // explicitly typed or wont work
  val shapes: List[Shape] = List(
    Rectangle(5.0, 6.0),
    Circle(5.0)
  )

  writeCsv(shapes)

  /////////////////////////////////////////////
  // CsvEncoder[Tree[Int]]

  // Why resolution is not converging ?
  //
  // If the compiler sees the same type constructor twice and the complexity of the type parameters is
  // increasing, it assumes that branch of search is “diverging”.
  //
  // 1 CsvEncoder[Tree[Int]]
  // 2 CsvEncoder[Branch[Int] :+: Leaf[Int] :+: CNil]
  // 3 CsvEncoder[Branch[Int]]
  // 4 CsvEncoder[Tree[Int] :: Tree[Int] :: HNil]
  // 5 CsvEncoder[Tree[Int]]

  // case class Bar(baz: Int, qux: String)
  // case class Foo(bar: Bar)

  // 1 CsvEncoder[Foo]
  // 2 CsvEncoder[Bar :: HNil]
  // 3 CsvEncoder[Bar]
  // 4 CsvEncoder[Int :: String :: HNil]

  CsvEncoder[Tree[Double]]

  // Sometimes your implicits resolution fails. The best way to debug it
  // is going from most general to less general:

  // case class Foo(a: Int, b: Boolean)
  // CsvEncoder[Foo] // this fails
  // CsvEncoder[Int :: Boolean :: HNil] // this fails
  // CsvEncoder[Int] // this does not fail
  // CsvEncoder[Boolean] // this fails then it's the root of the problem

  import scala.reflect.runtime.universe.{Tree => _, _}

  // this should show a full AST of the types

  println(reify {
    CsvEncoder[Tree[Double]]
  })
}
