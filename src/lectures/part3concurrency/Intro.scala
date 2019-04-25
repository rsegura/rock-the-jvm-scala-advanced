package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {


  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  aThread.start() //gives the signal to the JVM to tstart a JVM thread
  //create a JVM thread

  aThread.join() //blocks until aThread finished running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))

  val threadGoodBye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  threadHello.start()
  threadGoodBye.start()
  //different runs produce different results!


  //executors

  /*val pool = Executors.newFixedThreadPool(10)
 pool.execute(() =>println("something in the thread pool"))

  pool.execute(() =>{
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() =>{
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })
  pool.shutdown()*/

  //pool.shutdownNow()

  def runInParallel :Unit ={
    var x = 0

    val thread1 = new Thread(() =>{
      x = 1
    })
    val thread2 = new Thread(() =>{
      x = 1
    })
  }
  //for (_ <- 1 to 10000) runInParallel

  class BankAccount (var amount: Int){
    override def toString: String = ""+ amount
  }

  def buy (account: BankAccount, thing:String, price: Int) :Unit = {
    account.amount -=price
    //println("I've bought "+thing)
    //println("My account is now " +account)
  }

  /*for (_ <- 1 to 1000){
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iPhone", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if(account.amount != 43000) println("AHA " +account.amount)
    //println()


    /*
      thread1 (shoes) :50000
        - account  = 50000 - 3000 = 47000
      thread2 (iphone) : 50000
        - acoount = 50000 - 4000 = 46000 overwrites the memory of account.amount
     */

    //option #1: use synchronized
    def buySafe(account: BankAccount, thing: String, price: Int): Unit =
      account.synchronized({
        account.amount -= price
        println("I've bought "+thing)
        println("My account is now " +account)
      })



  }*/

  /**
    * Exercises
    * 1) Construct 50 "inception" threads
    *     Thread1 -> Thread2 -> threa3 -> ...
    *     println("hello from thread #"=
    *     in REVERSE ORDER
    *
    * 2) var x = 0
    *    val threads = (1 to 100).map(_ => new Thread(() => x +=1))
    *    threads.foreach(_.start())
    *
    *    1) what is the biggest value possible for x? 100
    *    2) what is the smallest value possible for x? 1
    *
    *3) sleep fallacy
    *
    *   var message = ""
    *   val awesomeThread = new Thread(() => {
    *     Thread.sleep(1000)
    *     message = "Scala is awesome"
    *   })
    *
    *   message = "Scala sucks"
    *   awesomeThread.start()
    *   Thread.sleep(2000)
    *   println(message)
    *
    *   1) what is the value of message?
    *       is it guaranteed?
    *       why? why not?
    */



  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = new Thread(() =>{
    if ( i < maxThreads ){
      val newThread = inceptionThreads(maxThreads, i + 1)
      newThread.start()
      newThread.join()
    }
    println(s"Hello from thread $i")
  })

  inceptionThreads(50).start()

}
