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

  //part 3

  implicit class HTMLEnrichment[T](value: T){
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(john.toHTML(UserSerializer)) // println( new HTMLENRICHMENT[User](john).toHTML(UserSerializer))
  println(john.toHTML)
  //COOL!
  /*
   - extend to new types
   - choose implementation
   - super expressive!
   */

  println(42.toHTML)
  println(john.toHTML(PartialUserSerializer))

  /*
   - type class itself --- HTMLSerializer[T]{ .. }
   - type class instances (some of which are implicits) -- UserSerializer, IntSerializer
   - conversion with implicit classes -- HTMLEnrichment
   */

  //context bounds
  def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]) :String =
    s"<html><body> ${content.toHTML(serializer)}<body><html>"

  def htmlSugar[T: HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    s"<html><body> ${content.toHTML(serializer)}<body><html>"
  }

  //implicitly

  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("0744")

  //in some other part of the code
  val standardPerms = implicitly[Permissions]


}
