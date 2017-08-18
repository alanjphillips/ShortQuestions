import org.scalatest.{Matchers, WordSpec}

import AssortedUtil._

class AssortedUtilSpec extends WordSpec with Matchers {

  "AssortedUtil" should {

    "return given number of largest elements from List" in {
      val list = List(8, 5, 3, 6, 2, 9)
      largestElementsDecreasing(list, 4) shouldBe List(9, 8, 6, 5)
    }

    "reverse a LinkedList into groups of a given size" in {
      val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      reverseLinkedListGroups(list, 3) shouldBe List(3, 2, 1, 6, 5, 4, 9, 8, 7)
    }

    "return a list containing addition result of 2 lists" in {
      val listA = List(5, 4)
      val listB = List(5, 4, 3)
      addLinkedListNumbers(listA, listB) shouldBe List(0, 9 ,3)
    }

    "create a Vector of next larger elements" in {
      val vec = Vector(1, 3, 2, 4)
      nextLargerElement(vec) shouldBe Vector(3, 4, 4, -1)
    }

    "first singleton" in {
      firstSingleton(List(8, 2, 3, 2, 1, 8, 7, 2)) shouldBe Some(3)
    }

    "get existing user" in {
      val db = Map(5 -> User(5, "user5"), 6 -> User(6, "user6"))
      retrieveUser(5, db) shouldBe Some(User(5, "user5"))
    }

    "get non existing user" in {
      val db = Map(5 -> User(5, "user5"), 6 -> User(6, "user6"))
      retrieveUser(7, db) shouldBe None
    }

  }

}