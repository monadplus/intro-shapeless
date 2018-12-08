import CsvEncoder._

object DerivingProduct extends App {

  val iceCreams: List[IceCream] = List(
    IceCream("vanilla", inCone = true),
    IceCream("chocolate", inCone = true),
    IceCream("strawberry", inCone = false)
  )

  /** Product auto derivation */
  import shapeless.{::, HList, HNil}

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

  import shapeless.Generic

  // one per each product type ?
  implicit lazy val iceCreamEncoder: CsvEnconder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    instance(iceCream => enc.encode(gen.to(iceCream)))
  }
  writeCsv(iceCreams)(iceCreamEncoder)

  case class Wine(name: String, isOpen: Boolean)
  val wines = List(
    Wine("Brunello", false),
    Wine("Blanc de Noirs", false),
    Wine("Codorniu", false)
  )

  implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R], // Generic[A] { type Repr = R },
      enc: CsvEnconder[R]
  ): CsvEnconder[A] =
    instance(a => enc.encode(gen.to(a)))

  writeCsv(wines)
  /* Expands to:

  writeCsv(iceCreams)(
    genericEncoder(
      Generic[IceCream],
        hlistEncoder(stringEncoder,
          hlistEncoder(intEncoder,
            hlistEncoder(booleanEncoder, hnilEncoder)))))
 */

}
