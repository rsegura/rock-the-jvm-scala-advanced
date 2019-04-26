package lectures.part3concurrency

import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

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
}
