import Model._

/*
 *  Be smart, do not do that
 */
object InstancesByHand {
  implicit lazy val IceCreamCsvEnconder: CsvEnconder[IceCream] = new CsvEnconder[IceCream] {
    override def encode(ice: IceCream): List[String] =
      List(
        ice.name,
        if (ice.inCone) "in cone" else "in cup"
      )
  }

  implicit lazy val EmployeeCsvEncoder: CsvEnconder[Employee] = new CsvEnconder[Employee] {
    override def encode(emp: Employee): List[String] =
      List(
        emp.name,
        if (emp.isIntern) "intern" else "not intern"
      )
  }
}
