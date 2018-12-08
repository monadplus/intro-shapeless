/*
  Definition of all case classes and companion objects
 */

case class IceCream(name: String, inCone: Boolean)
//object IceCream {
//  implicit lazy val IceCreamCsvEnconder = new CsvEnconder[IceCream] {
//    override def encode(ice: IceCream): List[String] =
//      List(
//        ice.name,
//        if (ice.inCone) "in cone" else "in cup"
//      )
//  }
//}

case class Employee(name: String, isIntern: Boolean)
//object Employee {
//  implicit lazy val EmployeeCsvEncoder = new CsvEnconder[Employee] {
//    override def encode(emp: Employee): List[String] =
//      List(
//        emp.name,
//        if (emp.isIntern) "intern" else "not slave"
//      )
//  }
//}

sealed trait Shape
//object Shape {
//  implicit lazy val ShapeEncoder: CsvEnconder[Shape] = new CsvEnconder[Shape] {
//    override def encode(value: Shape): List[String] = value match {
//      case Rectangle(width, height) => List(width.toString, height.toString)
//      case Circle(radius)           => List(radius.toString)
//    }
//  }
//}
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double)                   extends Shape

sealed trait Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A)                        extends Tree[A]
