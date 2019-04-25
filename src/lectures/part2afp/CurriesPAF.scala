package lectures.part2afp

object CurriesPAF extends App {
  //curried functions
  val superAdder: Int => Int => Int = x => y => x+y

  val add3 = superAdder(3) // Int => Int = y=> 3+y
  println(add3(5))
  println(superAdder(3)(5))  //curried function

  def curriedAdder(x: Int)(y: Int) : Int = x+y //curried method

  val add4 : Int => Int = curriedAdder(4) //converting a method into a function value

  //lifting =ETA-Expansion

  //functions != methods (JVM limitation)

  def inc(x: Int) : Int = x +1
  List(1,2,3).map(x => inc(x))  //ETA_expansion

  // Partial Function applications
  val add5= curriedAdder(5) _ // => Int => Int

  // Exercise
  val simpleAddFunction = (x: Int, y: Int) => x+y
  def simpleAddMethod(x: Int, y: Int) : Int = x+y
  def curriedAddMethod(x: Int)(y: Int) : Int = x+y


  //add7 : Int => Int = y => 7 +y
  // as many different implementations of add7 using the above
  //be creative!
  val add7 = (x:Int) => simpleAddFunction(7, x) //simplest
  val add7_3 = simpleAddFunction.curried(7)
  val add7_2 = curriedAddMethod(7) _ //PAF
  val add7_4 = curriedAddMethod(7)(_) // PAF  = alternative syntax

  val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
      //y=>simpleAddMethod(7,y)

  val add7_6 = simpleAddMethod(7, _:Int)

  //undersocore are powerful
  def concatenator(a:String, b:String, c:String): String = a+b+c
  val insertName = concatenator("Hello, I'm ", _:String, ", how are you?")
  println(insertName("Robert"))

  val fillInTheBlanks = concatenator("Hello: ", _:String, _:String)
  println(fillInTheBlanks("Robert", " Scala is awesome!"))

  /*
  Exercises
    1. Process a list of numbers and return their string represnetations with different formats
      use the %4.2f, %8.6 and %14.12f with a curried formatter function.
   */

  def curriedFormatter (s:String)(number:Double): String = s.format(number)
  def transformer(a: String, b: Double): String = a.format(b)
  val numbers = List(Math.PI, Math.E, 1238.233423, 1.3e-12, 4)

  val simpleFormat = transformer("%4.2f", _:Double)
  val seriousFormat = transformer("%8.6f", _:Double)
  val preciseFormat = transformer("%14.12f", _:Double)

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))
  //List(1.0,2.3423,355.34928374).map(transformer("%4.2f ", _:Double))



}
