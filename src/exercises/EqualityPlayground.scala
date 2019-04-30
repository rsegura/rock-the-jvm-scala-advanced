package exercises

import lectures.part4Implicits.TypeClasses.User

object EqualityPlayground extends App {

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer : Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

  implicit object NameEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }
  implicit object IntEquality extends Equal[Int]{
    override def apply(a: Int, b: Int): Boolean = a.equals(b)
  }

  val robert2 = User("Robert", 33, "test2@example.com")
  val robert = User("Robert", 32, "test@example.com")
  val john = User("John", 32, "john@rockthejvm.com")


  println(Equal(42, 42))
  println(Equal(john, robert))
//AD-HOC polymorphism ----^^


  /*
  Exercise - improve the Equal TC with an implicit conversion class
  1º === (anotherValue: T)
  2º !==(anotherValue: T)
   */

  implicit class TypeSafeEqual[T](value: T){
    def ===(anotherValue: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(value, anotherValue)
    def !==(anotherValue: T)(implicit equalizer: Equal[T]): Boolean = !equalizer.apply(value, anotherValue)
  }

  println(robert === robert2)
  println(42 === 34)
  println(robert.=== (robert2)(FullEquality))
  /*
  TYPE SAFE
   */
   //println(john === 43)

}
