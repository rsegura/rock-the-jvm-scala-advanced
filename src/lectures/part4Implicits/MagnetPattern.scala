package lectures.part4Implicits

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MagnetPattern extends App {


  //method overloading Problems

  class P2PRequest
  class P2PResponse
  class Serializer[T]

  trait Actor{
    def receiver(statusCode: Int): Int
    def receiver(request: P2PRequest): Int
    def receiver(request: P2PResponse): Int
    def receiver[T: Serializer](message: T): Int
    def receiver[T: Serializer](message: T, statusCode: Int): Int
    def receiver(future: Future[P2PResponse]): Int
    // def receive(future: Future[P2PResponse]): Int
  }
  /**
    * ------Problems-------
    * 1- Type erasure
    * 2- lifting doesn't work for all overloads
    * 3- code duplication
    * 4- type inference and defaults args not works fine
   */


  trait MessageMagnet[Result]{
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int]{
    def apply() : Int = {
      //logic for handling a P2PRequest
      println("Handling P2P request")
      42
    }
  }

  implicit class FromP2PResponse(request: P2PResponse) extends MessageMagnet[Int]{
    def apply() : Int = {
      //logic for handling a P2PResponse
      println("Handling P2P response")
      24
    }
  }

  receive(new P2PRequest)
  receive(new P2PResponse)

  /**
    * Benefits of Magnet Pattern
    * 1- no more type erasuer problems!
    */
  implicit class FromResponseFuture(future: Future[P2PResponse]) extends MessageMagnet[Int]{
    def apply() :Int = 2
  }

  implicit class FromRequestFuture(future: Future[P2PRequest]) extends MessageMagnet[Int]{
    def apply() :Int = 3
  }

  println(receive(Future(new P2PRequest)))
  println(receive(Future(new P2PResponse)))

  /**
    * Benefits of Magnet Pattern
    * 2- lifting works
    */

  trait MathLib{
    def add1(x:Int) = x+1
    def add1(x:String) = x.toInt +1
    //add1 overloads
  }

  //"magnetize"

  trait AddMagnet{
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet{
    override def apply(): Int = x +1
  }

  implicit class AddString(x: String) extends AddMagnet{
    override def apply(): Int = x.toInt + 1
  }

  val addFV = add1 _
  println(addFV(1))
  println(addFV("3"))

}
