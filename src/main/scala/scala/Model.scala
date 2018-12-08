object Model {
  case class IceCream(name: String, inCone: Boolean)
  case class Employee(name: String, isIntern: Boolean)

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double)                   extends Shape

  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]
}