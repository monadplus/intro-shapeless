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

object DerivingProduct extends App {

  import shapeless.{::, Generic, HList, HNil}

  implicit val booleanEncoder: CsvEnconder[Boolean] =
    instance(b => if (b) List("yes") else List("no"))

  implicit val stringEncoder: CsvEnconder[String] =
    instance(str => List(str))

  implicit val hnilEncoder: CsvEnconder[HNil] =
    instance { _ =>
      Nil
    }

  implicit def hlistEncoder[H, T <: HList](implicit
                                           hEncoder: CsvEnconder[H],
                                           tEncoder: CsvEnconder[T]): CsvEnconder[H :: T] =
    instance {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit lazy val iceCreamEncoder: CsvEnconder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    instance(iceCream => enc.encode(gen.to(iceCream)))
  }

  val iceCreams: List[IceCream] = List(
    IceCream("vanilla", inCone = true),
    IceCream("chocolate", inCone = true),
    IceCream("strawberry", inCone = false)
  )

  scala.Console.println {
    s"Ice creams\n${writeCsv(iceCreams)(iceCreamEncoder)}\n"
  }

  // --------------------------------------------------------

  case class Wine(name: String, isOpen: Boolean)
  val wines = List(
    Wine("Brunello", false),
    Wine("Blanc de Noirs", false),
    Wine("Codorniu", false)
  )

  implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R],
      enc: CsvEnconder[R]
  ): CsvEnconder[A] =
    instance(a => enc.encode(gen.to(a)))

  /* Expands to:
  writeCsv(iceCreams)(
    genericEncoder(
      Generic[IceCream],
        hlistEncoder(stringEncoder,
            hlistEncoder(booleanEncoder, hnilEncoder)))))
 */

  scala.Console.println {
    s"Wines\n${writeCsv(wines)}"
  }
}