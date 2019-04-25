package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication  extends App {


  /*
  The producer-consumer prblem
  producer -> [ ? ] -> consumer
   */

  class SimpleContainer {
    private var value : Int = 0

    def isEmpty : Boolean = value == 0
    def set(newValue: Int) :Unit = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting....")
      while(container.isEmpty){
        println("[consumer] actively waiting....")
      }
      println("[consumer] I have consumed " +container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing....")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work, the value " + value)
      container.set(value)
    })
    consumer.start()
    producer.start()
  }

  //naiveProdCons()

  //wait and notify
  def smartProdcCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting....")
      container.synchronized{
        container.wait()
      }
      println("[consumer] I have consumed " +container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work....")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

  //smartProdcCons()

  /*
  producer -> [ ? ? ? ] -> consumer

   */

  def prodConsLargeBuffer(): Unit = {
    val buffer : mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() =>{
      val random = new Random()

      while(true){
        buffer.synchronized{
          if(buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }

          //there must be at least one value in the buffer
          val x = buffer.dequeue()
          println("[consumer] consumed " +x)


          buffer.notify()

        }
        Thread.sleep(random.nextInt(500))

      }
    })
    val producer = new Thread(() => {
      val random = Random
      var i = 0

      while (true) {
        buffer.synchronized{
          if(buffer.size == capacity) {
            println("[producer] buffer is full, waiting....")
            buffer.wait()
          }

          //there must be at least one empty space in the buffer
          println("[produced] producing " +i)
          buffer.enqueue(i)

          buffer.notify()
           i += 1

        }

        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

  //prodConsLargeBuffer()





  /*
  Prod-cons, level 3

   producer1 -> [ ? ? ? ] -> consumer1
   procuder2 -----^    ^------ consumer2

   */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread{

    override def run() :Unit = {
      val random = new Random()

      while(true){
        buffer.synchronized{
          while(buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }

          //there must be at least one value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed " +x)


          buffer.notify()

        }
        Thread.sleep(random.nextInt(500))

      }
    }
  }

  class Producer (id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread{
    override def run(): Unit = {
      val random = Random
      var i = 0

      while (true) {
        buffer.synchronized{
          while(buffer.size == capacity) {
            println(s"[producer $id] buffer is full, waiting....")
            buffer.wait()
          }

          //there must be at least one empty space in the buffer
          println(s"[produced $id] producing " +i)
          buffer.enqueue(i)

          buffer.notify()
          i += 1

        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }


  def multiProdCons (nConsumers: Int, nProducer: Int) : Unit = {
    val buffer : mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 20

    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
    (1 to nConsumers).foreach(i => new Producer(i, buffer, capacity).start())
  }

  //multiProdCons(3, 6)


    /*
    Exercises.
    1) Think of an example where notifyAll acts in a different way than notify?
    2) create a deadlock
    3) create a livelock
     */


  //notifyAll
  def testNotifyAll() :Unit = {
    val bell = new Object
    (1 to 10).foreach(i => new Thread(() =>{
      bell.synchronized{
        println(s"[thread $i] waiting...")
        bell.wait()
        println(s"[thread $i] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock'n roll!")
      bell.synchronized{
        bell.notifyAll()
      }
    }).start()
  }


  //testNotifyAll()


  //2 - deadlock

  case class Friend(name:String){
    def bow(other: Friend): Unit ={
      this.synchronized{
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }


    def rise(other: Friend) :Unit = {
      this.synchronized{
        println(s"$this: I am rising to my friend $other")
      }
    }

    var side = "right"

    def switchSide(): Unit ={
      if(side == "right") side = "left"
      else side = "right"
    }

    def pass (other: Friend) : Unit = {
      while(this.side == other.side) {
        println(s"$this: Oh, but please, $other, feel free to pass...")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }


  val sam = Friend("Sam")

  val pierre = Friend("Pierre")

  //new Thread(() => sam.bow(pierre)).start()
  //new Thread(() => pierre.bow(sam)).start()


  // 3- livelock
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()


}
