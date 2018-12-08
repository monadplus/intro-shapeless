package types

import shapeless.HNil
import shapeless.syntax.singleton._

object TypesExplained extends App {

  /** Literal Types */
  // "hello" has 3 types:
  "hello": String
  "hello": AnyRef
  "hello": Any
  // but it actually have 4 types: a singleton type

  object Foo
  // Foo.type: is the type of Foo and the only value with that type

  // Singleton types applied to literal values are called literal types.

  // compiler widen literals to their nearest non-singleton type

  //  x = 43  // it can only contain 42 literal
  // However x is still a subtype of Int: you should be able to sum (I cant)

  1.narrow
  true.narrow
  "hello".narrow

//   math.sqrt(4).narrow // can't narrow compound types

//   scala added this types at: Lightbend 2.11.9 and Typelevel 2.11.8
//   scalac: -Ypartial-types
//  val theAnswer: 42 = 42

  /** Type tagging and phantom types */
  val number = 42 // Int in two world: runtime: actual value and methods
  //                   compile-time: compiler uses the type

  /** phantom types */
  // types with no run-time semantics:
  trait Cherries
  val numCherries = number.asInstanceOf[Int with Cherries]

  // Shapeless uses this   trick to tag fields and subtypes in an ADT with
  // the singleton types of their names

  val someNumber   = 123
  val numCherries2 = "numCherries" ->> someNumber // KeyTag["numCherries", Int]

  import shapeless.labelled.field
  field[Cherries](123)
  // type FieldType[K, V] = V with KeyTag[K, V]

  // Tags exist purely at compile time
  // Shapeless provides a type class called Witness.

  import shapeless.Witness
  import shapeless.labelled.FieldType

  val numCherries0 = "numCherries" ->> 123

  // Get the tag from a tagged value:
  def getFieldName[K, V](value: FieldType[K, V])(
      implicit witness: Witness.Aux[K]
  ): K =
    witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  println(
    s"name: ${getFieldName(numCherries0)}," +
      s" value: ${getFieldValue(numCherries0)}"
  )

  /** Labelled Generic */
  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

}
