package lectures.part1as

import scala.annotation.tailrec

object Recap extends App{
  val aCondition : Boolean = false
  val aConditionedVal = if(aCondition) 42 else 65

  //instructions vs expressions

  //compiler infers types for us
  val aCodeBlock = {
    if(aCondition) 54
    56
  }

  //recursion stack and tail

  @tailrec def factorial(n: Int, acc: Int) : Int =
    if(n <= 0) acc
    else factorial(n-1, n * acc)


  //object-oriented programing
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog //subtyping polymorphism


  //Abstrac class
  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore{
    override def eat (a: Animal): Unit = println("ñam")
  }

  //method notations

  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog //natural language


  //anonymous classes
  val aCarnivore = new Carnivore{ // a trait can't be estanciated but we can override his methods
    override def eat(a: Animal): Unit = println("roar!")
  } //The compiler extends and anonymous class for us

  //generics covariance
  abstract class MyList[+A]

  //singletons and companions
  object MyList

  //case classes
  case class Person(name: String, age: Int)

  //exceptions and try/catch/finally
  val throwsException = throw new RuntimeException // the type is Nothing
  val aFailure = try{
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }

  //packaging and imports
  //functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 +1
  }
  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1 //Syntactic sugar and anounymous function

  List(1,2,3).map(anonymousIncrementer) // HOF (map, flatMap, filter)

  // for-comprehension
  val pairs = for {
    num <- List(1,2,3)
    char <- List('a', 'b', 'c')
  } yield num+ "-"+char


  //Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Robert" -> 616
  )

  val anOption = Some(2)

  //pattern matching
  val x = 2
  val order = x match{
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  //all the patterns
}
