package lectures.part2afp

object PartialFunctions  extends App {

  val afunction = (x: Int) => x+1 // Function1[Int, Int] === Int => Int

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value equivalent with the above function (proper function)

  println(aPartialFunction(2))


  //PF utilites
  println(aPartialFunction.isDefinedAt(67))

  // lift
  val lifted = aPartialFunction.lift // Int => Option[Int] transform a partialFunction
  println(lifted(2))
  println(lifted(98))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  //PF extends normal functions

  val aTotalFunction : Int  => Int = {
    case 1 => 99
  }

  //HOFs accept partial functions as well

  val aMappedList = List(1,2,3).map{
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
  Note: PF can only have ONE parameter type
   */

  /**
    * Exercises
    *
    * 1 - construct a PF instance yourself (anonymous class)
    * 2- dubm chatbot as aPF
    */

  val aManualPartialFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 42
      case 2 => 65
      case 5 => 999
    }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val chatBot : PartialFunction[String, String] = {
    case "hello" => "Hi, my name is bash"
    case "goodbye" => "Bye Bye"
    case "call mom" => "Unable to find your phone"
    case _ => "Error"
  }

  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)

}
