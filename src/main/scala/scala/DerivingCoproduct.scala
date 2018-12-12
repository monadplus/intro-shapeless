import CsvEncoder._
import Model._

/*
1.- Define your type class
2.- Define instances for primitive types
3.- Define instances for HList (HNil, ::)
4.- Define instances for Coproduct (CNil, :+:)
5.- Define an instance for Generic
6.- Don't forget to add Lazy to :: and :+: header, Generic.Repr (Only required when your TC is complex or recursive)
 */

object DerivingCoproduct extends App {

  import shapeless.{::, HList, HNil, Lazy}
  import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

  implicit val doubleEncoder: CsvEnconder[Double] =
    instance(d => List(d.toString))

  implicit val cnilEncoder: CsvEnconder[CNil] =
    instance { _ =>
      throw new RuntimeException("Inconceivable")
    }

  implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                   hEncoder: CsvEnconder[H],
                                                   tEncoder: CsvEnconder[T]): CsvEnconder[H :+: T] =
    instance {
      case Inl(h) => hEncoder.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

  // Product encoder is required by coproduct encoder to produce the H of coproductEncoder
  // Example: Shape :: Rectangle :+: Circle :+: CNil
  //          Circle instance:  Inr(Inl(Circle))
  implicit def hlistEncoder[H, T <: HList](implicit
                                           hEncoder: CsvEnconder[H],
                                           tEncoder: CsvEnconder[T]): CsvEnconder[H :: T] =
    instance {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit val hnilEncoder: CsvEnconder[HNil] =
    instance { _ =>
      Nil
    }

  implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R],
      enc: Lazy[CsvEnconder[R]]
  ): CsvEnconder[A] =
    instance(a => enc.value.encode(gen.to(a)))

  // must be explicitly typed (implicits resolution)
  val shapes: List[Shape] = List(
    Rectangle(5.0, 6.0),
    Circle(5.0)
  )

  scala.Console.println {
    s"Printing shapes...\n${writeCsv(shapes)}"
  }

  // Will it work or not ?
//  val trees: List[Tree[Double]] = List(
//    Branch(Leaf(2.0), Branch(Leaf(4.0), Leaf(8.0))),
//    Leaf(1.0)
//  )
//
//  scala.Console.println {
//    s"Printing trees...\n${writeCsv(trees)}"
//  }
}
