package lectures.part3concurrency

import scala.concurrent.Future
import scala.util.{Failure, Success}

//importan for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesPromises  extends App {

  def calculateMeaningofLife : Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningofLife //Execute the operation on ANOTHER thread
  } //(global) which is passed by the compiler

  println(aFuture.value)

  println("Waiting on the future")
  aFuture.onComplete{
    case Success(meaningOfLife) => println(s"The meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  }

  Thread.sleep(3000)

}
