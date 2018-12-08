package types

object JsonExample extends App {

  import shapeless.Witness
  import shapeless.labelled.FieldType
  import shapeless.{::, HList, HNil, Lazy}

  sealed trait JsValue
  case class JsObject(fields: List[(String, JsValue)]) extends JsValue
  case class JsArray(values: List[JsValue])            extends JsValue
  case class JsString(value: String)                   extends JsValue
  case class JsNumber(value: Double)                   extends JsValue
  case class JsBool(value: Boolean)                    extends JsValue
  case object JsNull                                   extends JsValue

  trait JsEncoder[A] {
    def encode(value: A): JsValue
  }

  object JsEncoder {
    def apply[A](implicit enc: JsEncoder[A]): JsEncoder[A] = enc
    def instance[A](f: A => JsValue): JsEncoder[A] = new JsEncoder[A] {
      override def encode(value: A): JsValue = f(value)
    }

    implicit val stringEncoder: JsEncoder[String] = instance(JsString)
    implicit val intEncoder: JsEncoder[Int]       = instance(JsNumber(_))
    implicit val doubleEncoder: JsEncoder[Double] = instance(JsNumber)
    implicit val boolEncoder: JsEncoder[Boolean]  = instance(JsBool)
    implicit def listEncoder[A](implicit enc: JsEncoder[A]): JsEncoder[List[A]] =
      instance(list => JsArray(list.map(enc.encode)))
    implicit def optionEncoder[A](implicit enc: JsEncoder[A]): JsEncoder[Option[A]] =
      instance(opt => opt.fold[JsValue](JsNull)(enc.encode))
  }

  import JsEncoder._

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  case class Store(name: String, products: List[String], capacity: Option[Int])

  val iceCream = IceCream("vanilla", 4, inCone = true)
  val store    = Store("Dream Land", List("Toy A", "Toy B"), Some(10000))

  // ideally we'd like to produce something like this:
//  val iceCreamJson = JsObject(List(
//    "name" -> JsString("vanilla"),
//    "numCherris" -> JsNumber(4),
//    "inCone" -> JsBool(true)
//  ))

  import shapeless.LabelledGeneric

  val labelledGen = LabelledGeneric[IceCream]
  val gen         = labelledGen.to(iceCream)

  // String with KeyTag[Symbol    with Tagged["name"],  String] ::
  // Int with KeyTag[Symbol    with Tagged["numCherries"], Int] ::
  // Boolean with KeyTag[Symbol with Tagged["inCone"], Boolean] ::
  // HNil

  trait JsObjectEncoder[A] extends JsEncoder[A] {
    def encode(value: A): JsObject
  }

  object JsObjectEncoder {
    def objectInstance[A](f: A => JsObject): JsObjectEncoder[A] = new JsObjectEncoder[A] {
      override def encode(value: A): JsObject = f(value)
    }
  }

  import JsObjectEncoder._

  implicit val hnilEncoder: JsObjectEncoder[HNil] =
    objectInstance(hnil => JsObject(Nil))

//  implicit def hlistObjectEncoder[H, T <: HList](
//                                                implicit
//                                                hEncoder: Lazy[JsEncoder[H]],
//                                                tEncoder: JsObjectEncoder[T]
//                                                ): JsEncoder[H :: T] = ???

//  implicit def hlistObjectEncoder[K, H, T <: HList](
//                                                implicit
//                                                hEncoder: Lazy[JsEncoder[H]],
//                                                tEncoder: JsObjectEncoder[T]
//                                              ): JsObjectEncoder[FieldType[K, H] :: T] = ???

//  implicit def hlistObjectEncoder[K, H, T <: HList](
//                                                     implicit
//                                                     witness: Witness.Aux[K],
//                                                     hEncoder: Lazy[JsEncoder[H]],
//                                                     tEncoder: JsObjectEncoder[T]
//                                                   ): JsObjectEncoder[FieldType[K, H] :: T] = {
//    val fieldName = witness.value
//    ???
//  }
  implicit def hlistObjectEncoder[H, K <: Symbol, T <: HList](
      implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsEncoder[H]],
      tEncoder: JsObjectEncoder[T]
  ): JsObjectEncoder[FieldType[K, H] :: T] =
    objectInstance { hlist =>
      val fieldName = witness.value.name
      val head      = hEncoder.value.encode(hlist.head)
      val tail      = tEncoder.encode(hlist.tail)
      JsObject((fieldName -> head) :: tail.fields)
    }

  implicit def genericObjectEncoder[A, H](
      implicit
      gen: LabelledGeneric.Aux[A, H],
      enc: Lazy[JsObjectEncoder[H]]
  ): JsEncoder[A] =
    instance { value =>
      enc.value.encode(gen.to(value))
    }

  println(JsEncoder[IceCream].encode(iceCream))
  println(JsEncoder[Store].encode(store))

  ////////////////////////////////////
  ////////////////////////////////////
  /////////// Coproduct  /////////////
  ////////////////////////////////////
  ////////////////////////////////////
  ////////////////////////////////////

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double)                   extends Shape

  LabelledGeneric[Shape].to(Circle(1.0))

  // Rectangle with KeyTag[Symbol with Tagged["Rectangle", Rectangle]
  // :+:
  // Circle with KeyTag[Symbol with Tagged["Circle", Circle]
  // :+:
  // CNil

  import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy, Witness}
  import shapeless.labelled.FieldType

  implicit val cnilObjectEncoder: JsObjectEncoder[CNil] =
    objectInstance(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
      implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsEncoder[H]],
      tEncoder: JsObjectEncoder[T]
  ): JsObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    objectInstance {
      case Inl(head) =>
        JsObject(List(typeName -> hEncoder.value.encode(head)))
      case Inr(tail) =>
        tEncoder.encode(tail)
    }
  }

  val shape: Shape = Circle(1.0)
  val jsonShape    = JsEncoder[Shape].encode(shape)
  println(jsonShape)
}
