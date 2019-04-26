package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._

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

  //mini social network

  case class Profile(id: String, name: String){
    def poke(anotherProfile: Profile): Unit =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    //"database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()


    //API
    def fetchProfile (id: String): Future[Profile] = Future{
      // Fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future{
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }



  //client: mark to poke bill

  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete{
    case Success(markProfile) => {
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete{
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    }
    case Failure(ex) => ex.printStackTrace()
  }

  Thread.sleep(1000)


  //functional compositions of futures
  //map, flatMap, filter

  val nameOnTheWall = mark.map(profile => profile.name)

  val marksBestFriends = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))

  val zucksBestFriendsRestricted = marksBestFriends.filter(profile => profile.name.startsWith("Z"))


  //for-comprehensions

  for{
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")

    bill <- SocialNetwork.fetchBestFriend(mark)

  } yield{
    println(mark.name)
    mark.poke(bill)
  }
  Thread.sleep(1000)

  //fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover{
    case e: Throwable =>Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith{
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))



  //Online banking app blocking futures

  case class User(name: String)
  case class Transaction(sender:String, receiver: String, amount: Double, status: String)

  object BankingApp{
    val name = "Rock the JVM banking"

    def fetchUser(name: String) :Future[User] = Future{
      //simulate fetching from DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction (user: User, merchantName :String, amount: Double): Future[Transaction] = Future{

      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      //fetch the user from DB
      //create a transaction
      //wait for the transaction to finish

      val transactionStatusFuture = for{
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)

      } yield {
        transaction.status
      }
      Await.result(transactionStatusFuture, 2.seconds)
    }
  }

  println(BankingApp.purchase("Daniel", "iphone", "rock the jvm store", 3000))


  //promises

  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future


  // thread 1 - "consumer"
  future.onComplete{
    case Success(r) => println("[consumer] I've received "+r)
  }

  // thread 2 - "producer"
  val producer = new Thread(() =>{
    println("[producer] crunching numbers....")
    Thread.sleep(500)

    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)
}
