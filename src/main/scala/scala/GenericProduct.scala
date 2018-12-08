package scala

import shapeless._

object GenericProduct extends App {
  val product =
    "Arnau" :: 23 :: HNil

  val first: String =
    product.head
  val second: Int =
    product.tail.head

  val newProduct = 45L :: product

  //////////////////////////////////

  import shapeless.Generic

  val iceCream                           = IceCream("vanilla", inCone = true)
  val genericIceCream: Generic[IceCream] = Generic[IceCream]
  val repr                               = genericIceCream.to(iceCream)
  val iceCream2                          = genericIceCream.from(repr)

  val employeeFromGenericIceCream =
    Generic[Employee].from(Generic[IceCream].to(iceCream))

}
