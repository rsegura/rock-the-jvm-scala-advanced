package lectures.part4Implicits

import java.{ util => ju}

object ScalaJAvaConversions extends App {

  import collection.JavaConverters._

  val javaSet: ju.Set[Int] = new ju.HashSet[Int]()
  (1 to 5).foreach(javaSet.add(_))
  println(javaSet)

  val scalaSet = javaSet.asScala
}
