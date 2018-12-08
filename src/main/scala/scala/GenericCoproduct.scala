package scala

object GenericCoproduct extends App {
  import shapeless.{:+:, CNil, Inl, Inr}

  case class Red()
  case class Green()
  case class Blue()

  type Light = Red :+: Green :+: Blue :+: CNil

  val red: Light  = Inl(Red())
  val blue: Light = Inr(Inr(Inl(Blue())))

  import shapeless.Generic

  val gen = Generic[Shape]
  gen.to(Rectangle(2.0, 3.0))
  gen.from(gen.to(Circle(5.0)))

}
