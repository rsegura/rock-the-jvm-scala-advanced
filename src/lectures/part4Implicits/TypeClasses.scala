package lectures.part4Implicits

object TypeClasses extends App {

  case class User(name: String, age: Int, email: String)
  //best approach
  trait HTMLSerializer[T] {
    def serializer(value: T): String
  }

  object UserSerializer extends HTMLSerializer[User] {
    def serializer(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email} /> </div>"
  }

  //1 we can define serializers for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    def serializer (date: Date): String = s"<div>${date.toString()}</div>"
  }

  //2 we can define multiple serializer
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serializer(user: User): String = s"<div>${user.name} </div>"
  }

  //Type class loke like that
  trait MyTypeClassTemplate[T]{
    def action(vaue: T): String
  }

  /**
    * Implement Equality
    */

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object NameEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }
}
