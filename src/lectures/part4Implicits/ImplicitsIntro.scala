package lectures.part4Implicits

object ImplicitsIntro extends App {

  case class Person (name: String) {
    def greet = s"Hi, my name is $name!"
  }

  implicit def fromStringtoPerson(str: String): Person = Person(str)

  println("Peter".greet)

  //Implicit parameters
  def increment (x: Int)(implicit amount: Int) :Int = x+amount
  implicit val defaultAmount = 10
  increment(2)


}
