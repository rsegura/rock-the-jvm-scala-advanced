package exercises

import scala.annotation.tailrec

/*
Exercise : implement a lazily evaluated, singly linked STREAM of elements.
naturals = MyStream.from(1)(x => x +1) = stream of natural numbers (potentially infinite!)
naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
naturals.map(_ * 2)// stream of all even numbers(potentially infinite)

 */
abstract class MyStream[+A] {
  def isEmpty : Boolean
  def head: A
  def tail : MyStream[A]

  def #:: [B >: A](element: B): MyStream[B]
  def ++ [B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit) : Unit
  def map[B](f: A => B) : MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean) : MyStream[A]

  def take(n : Int): MyStream[A] // takes the first n elements out of this stream


  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)


}

object EmptyStream extends MyStream[Nothing]{
  def isEmpty : Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail : MyStream[Nothing] = throw new NoSuchElementException

  def #:: [B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
  def ++ [B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit) : Unit = ()
  def map[B](f: Nothing => B) : MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean) : MyStream[Nothing] = this

  def take(n : Int): MyStream[Nothing] = this // takes the first n elements out of this stream

}

class Cons[+A] (hd: A, tl : => MyStream[A]) extends MyStream[A]{
  def isEmpty : Boolean = false
  override val  head: A = hd
  override lazy val tail : MyStream[A] = tl

  def #:: [B >: A](element: B): MyStream[B] = new Cons[B](element, this)
  def ++ [B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  def foreach(f: A => Unit) : Unit = {
    f(head)
    tail.foreach(f)
  }
  def map[B](f: A => B) : MyStream[B] = new Cons(f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean) : MyStream[A] =
    if(predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  def take(n : Int): MyStream[A]  = // takes the first n elements out of this stream
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n-1))
}

object MyStream{
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator))
}


object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)
  println(naturals.take(10))
  println(naturals.flatMap(x => new Cons(x, new Cons(x +1, EmptyStream))).take(20))

  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new Cons[BigInt](first, fibonacci(second, first + second))

  println(fibonacci(1, 1).take(100).toList())

  def eratosthenes (numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons[Int](numbers.head, eratosthenes(numbers.tail.filter( _ %numbers.head != 0)))

  println(eratosthenes(MyStream.from(2)( _ +1)).take(100).toList())
}
