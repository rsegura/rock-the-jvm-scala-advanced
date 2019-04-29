package lectures.part4Implicits

object TypeClasses extends App {

  case class User(name: String, age: Int, email: String)
  //best approach
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email} /> </div>"
  }
  val robert = User("Robert", 32, "test@example.com")
  println(UserSerializer.serialize(robert))
  //1 we can define serializers for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    def serialize (date: Date): String = s"<div>${date.toString()}</div>"
  }

  //2 we can define multiple serializer
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} </div>"
  }

  //Type class loke like that
  trait MyTypeClassTemplate[T]{
    def action(value: T): String
  }



  //part 2
  object HTMLSerializer{
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  implicit object User2Serializer extends HTMLSerializer[User]{
    override def serialize(value: User): String = s"<p>This is a HTML test</p> for <h1>${value.name} with email -> ${value.email}</h1>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(john))

  //access to the entire type class
  println(HTMLSerializer[User].serialize(robert))

  /*
  Exercise: implement the TC pattern for the Equality TC.
   */

  /**
    * Implement Equality
    */

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer : Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

   object NameEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit object FullEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }
  implicit object IntEquality extends Equal[Int]{
    override def apply(a: Int, b: Int): Boolean = a.equals(b)
  }
  val robert2 = User("Robert", 33, "test2@example.com")

  println(Equal(42, 42))
  println(Equal(john, robert))
//AD-HOC polymorphism ----^^
}
