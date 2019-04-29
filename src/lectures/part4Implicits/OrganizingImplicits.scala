package lectures.part4Implicits

object OrganizingImplicits extends App {

    implicit val reverseOrdering : Ordering[Int] = Ordering.fromLessThan(_ > _)
    println(List(1, 5, 6, 2, 3).sorted)


    /*
    Implicits (used as implicit parameters):
      - val/var
      - object
      - accessor methods = defs with no parentheses
     */



    //Exercise

    case class Person(name: String, age: Int)

    val persons = List(
      Person("Steve", 30),
      Person("Amy", 22),
      Person("John", 66)
    )

  //IMPLEMENT ORDERING FOR PERSON
  //implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  //println(persons.sorted)

  //We can create a companion object for case class Person and add the implicit inside the object
  /*
    object Person {
      implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
    }

    Implicit scope
      - normal scope = LOCAL SCOPE
      - imported scope
      - companions of all types involved in the method signature
        - List
        - Ordering
        - all the types involved = A or any supertype
   */

  object AlphabeticNameOrdering {
    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  }

  import AgeOrdering._
  println(persons.sorted)

  /*
  Exercise
    - totalPrice = most Used (50%)
    - by unit count = 25%
    - by unit price = 25%
   */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase{
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)

  }
  object UnitCountOrderign {
    implicit val unitCountOrdering :Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits > b.nUnits)
  }
  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }
}
