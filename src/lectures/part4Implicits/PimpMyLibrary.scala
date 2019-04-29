package lectures.part4Implicits

object PimpMyLibrary extends App {


  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven:Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)

    def times(function: () => Unit): Unit = {
      def timesAux(n: Int): Unit =
        if (n <= 0) ()
        else {
          function()
          timesAux(n-1)
        }
      timesAux(value)
    }

    def *[T](list: List[T]): List[T] = {
      def concatenation(n : Int): List[T] =
        if(n < 0) List()
        else concatenation(n-1) ++ list

      concatenation(value)
    }
  }

  new RichInt(42).sqrt



  // type enrichment = pymping

  34.isEven
  12.sqrt
  1 to 10
  import scala.concurrent.duration._
  3.seconds

  /*
  Enrich the string class
  - asInt
  - encrypt Cesar code
      "John" ->Lnjp

  Enrich the Int class
    - times(function)
      3.times(() => ....)
    - *
      3 * List(1,2) => List(1,2,1,2,1,2)
   */

  implicit class RichString(val value: String) extends AnyVal{
    def asInt: Int = Integer.valueOf(value)
    def encrypt(cypherDistance: Int): String = value.map(c => (c + cypherDistance).asInstanceOf[Char])
  }

  println("Robert".encrypt(3))
  println("42".asInt)
  //println("test".asInt)

  3.times(() => println("Hello Scala!"))

  println(4 * List(1,2,3))

  // "3" / 4
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)

  println("8"/2)
}
