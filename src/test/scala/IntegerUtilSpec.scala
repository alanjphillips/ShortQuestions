import org.scalatest.{Matchers, WordSpec}

class IntegerUtilSpec extends WordSpec with Matchers {

  "IntegerUtil" should {

    "find the most frequent integer in multi-element array" in {
      val arr = Array(0, 1 , 1, 1, 2, 2)
      IntegerUtil.frequentInteger(arr) shouldBe 1
    }

    "find the most frequent integer in multi-element list" in {
      val lst = List(0, 1 , 1, 1, 2, 2)
      IntegerUtil.frequentInteger(lst) shouldBe 1
    }

    "find pairs in an integer list whose sum is equal to 10" in {
      val lst = List(1, 1, 9, 2, 4, 2, 8, 9, 6)
      IntegerUtil.findPairsForDistinct(lst, 10).size shouldBe 3
    }

    "find zero pairs in an integer list whose sum is equal to 10" in {
      val lst = List(1, 1, 5, 2, 4, 2, 4, 7)
      IntegerUtil.findPairsForDistinct(lst, 10).size shouldBe 0
    }

    "detect that list is rotated version of original list" in {
      val original = List(1, 2, 3, 4, 5, 6, 7, 8)
      val rotated = List(5, 6, 7, 8, 1, 2, 3, 4)

      IntegerUtil.isRotated(original, rotated) shouldBe true
    }

    "detect that list is not rotated version of original list" in {
      val original = List(1, 2, 3, 4, 5, 6, 7, 8)
      val rotated = List(5, 6, 7, 8, 1, 2, 4, 3)

      IntegerUtil.isRotated(original, rotated) shouldBe false
    }

    "detect that list is not rotated version of original list wrong size" in {
      val original = List(1, 2, 3, 4, 5, 6, 7, 8)
      val rotated = List(5, 6, 7, 8, 1, 2, 3)

      IntegerUtil.isRotated(original, rotated) shouldBe false
    }

    "find the only element in a list that only occurs once" in {
      val lst = List(1, 1, 9, 2, 4, 2, 9, 2)
      IntegerUtil.uniqueElement(lst) shouldBe Some(4)
    }

    "find that there are no elements in a list that only occurs once" in {
      val lst = List(1, 1, 9, 2, 4, 2, 8, 9, 4, 8)
      IntegerUtil.uniqueElement(lst) shouldBe None
    }

  }

}
