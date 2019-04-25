package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean){
/*
  EXERCISE - implement a functional set
 */
  def apply(elem: A): Boolean =
    contains(elem)
  def contains (elem: A) : Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A =>MySet[B]): MySet[B]
  def filter(predicate : A => Boolean) : MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  Exercise
  - removeing an element
  - intersection with another set
  - difference with another set
   */
  def -(elem: A): MySet[A]
  def &(anotherSet : MySet[A]): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]

  /*
  Exercise
  implement a unary_! = Negation of a set
   */
  def unary_! : MySet[A]

}

class EmptySet[A] extends MySet[A]{
  def contains (elem: A) : Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A =>MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate : A => Boolean) : MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def -(elem: A) : MySet[A] = this
  def &(anotherSet : MySet[A]): MySet[A] = this
  def --(anotherSet : MySet[A]): MySet[A] =  this
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
  def contains (elem: A) : Boolean = property(elem)
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A =>MySet[B]): MySet[B] = politelyFail
  def filter(predicate : A => Boolean) : MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = politelyFail

  def -(elem: A): MySet[A] = filter(x => x != elem)
  def &(anotherSet : MySet[A]): MySet[A] = filter(anotherSet)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains (elem: A) : Boolean =
    elem == head || tail.contains(elem)
  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  /*
  [1 2 3] ++ [4 5] =
  [2 3] ++ [4 5] + 1 =
  [3] ++ [4 5] + 1 + 2 =
  [] ++ [4 5] + 1 + 2 + 3
  [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
   */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A =>MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(predicate : A => Boolean) : MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  def -(elem: A): MySet[A] = {
    if (head == elem) tail
    else tail - elem + head
  }

  def &(anotherSet : MySet[A]) : MySet[A] = filter(x => anotherSet.contains(x))
  def --(anotherSet : MySet[A]) : MySet[A] = filter(x => !anotherSet.contains(x))

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet{
  /*
   val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
   = buildSet(seq(2,3), [] + 1)
   = buildSet(seq(3), [1] + 2)
   =buildSet(seq(), [1, 2] + 3)
   = [1, 2, 3]
   */
  def apply[A](value: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]) : MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(value.toSeq, new EmptySet[A])
  }
}


object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  s +5 ++ MySet( -1, 6) + 3 map (x => x*10) flatMap (x => MySet(x, x/2) filter (_ % 2 == 0)) foreach println

  val negative = !s  // s.unary_! = all the natural not equals to 1,2,3,4

  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_%2==0)
  println(negativeEven)


}
