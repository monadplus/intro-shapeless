import shapeless.{HNil, LabelledGeneric}
import shapeless.syntax.singleton._

import PhantomTypes.{getFieldName, getFieldValue}

object PhantomTypes extends {

  /** Literal Types */
//  ("hello": String)
//  ("hello": AnyRef)
//  ("hello": Any)
//  ("hello".narrow)

  /** Phantom Types: no run-time semantics */
  trait Cherries
  val number   = 1
  val numCherries0 = number.asInstanceOf[Int with Cherries]

  // Shapeless uses this trick to tag fields and subtypes in an ADT with the singleton types of their names
  "numCherries" ->> number // KeyTag["numCherries", Int]

  import shapeless.labelled.field
  field[Cherries](123) // type FieldType[K, V] = V with KeyTag[K, V]

  // Tags exist purely at compile time
  // Shapeless provides a type class called Witness.

  import shapeless.Witness
  import shapeless.labelled.FieldType

  def getFieldName[K, V](@unchecked value: FieldType[K, V])(
      implicit witness: Witness.Aux[K]
  ): K =
    witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  /** Labelled Generic */
  val garfield  = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
  // FieldType[SingletonOps#T, String] :: FieldType[SingletonOps#T, Boolean] :: HNil
}

object Main extends App {

  val numCherries = "numCherries" ->> 4
  println {
    s"name: ${getFieldName(numCherries)}," +
      s" value: ${getFieldValue(numCherries)}"
  }
}