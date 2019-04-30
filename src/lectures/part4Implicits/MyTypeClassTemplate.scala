package lectures.part4Implicits

 //TYPE CLASS
trait MyTypeClassTemplate[T]{
  def action(value: T): String
}

object MyTypeClassTemplate{
  def apply[T](implicit instance: MyTypeClassTemplate[T]): MyTypeClassTemplate[T] = instance
}