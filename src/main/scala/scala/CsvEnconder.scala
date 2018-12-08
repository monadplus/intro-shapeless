trait CsvEnconder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def writeCsv[A](values: List[A])(implicit enc: CsvEnconder[A]): String =
    values.map(a => enc.encode(a).mkString(",")).mkString("\n")

  implicit def pairEncoder[A, B](implicit aEncoder: CsvEnconder[A],
                                 bEncoder: CsvEnconder[B]): CsvEnconder[(A, B)] =
    new CsvEnconder[(A, B)] {
      override def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

  def apply[A](implicit enc: CsvEnconder[A]): CsvEnconder[A] =
    enc

  def instance[A](f: A => List[String]): CsvEnconder[A] =
    new CsvEnconder[A] {
      override def encode(a: A): List[String] = f(a)
    }
}