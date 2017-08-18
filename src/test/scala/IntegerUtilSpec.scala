import org.scalatest.{Matchers, WordSpec}

class IntegerUtilSpec extends WordSpec with Matchers {

  "IntegerUtil" should {

    "calculate the power given a base and exponent" in {
      IntegerUtil.power(2, 0) shouldBe 1
      IntegerUtil.power(2, 1) shouldBe 2
      IntegerUtil.power(2, 2) shouldBe 4
      IntegerUtil.power(2, 3) shouldBe 8
    }

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

    "find the common elements of 2 int arrays" in {
      val list1 = List(1, 2, 3, 4, 8, 9)
      val list2 = List(0, 3, 6, 5, 4, 7)
      IntegerUtil.commonElements(list1, list2).size shouldBe 2
    }

    "find zero common elements of 2 int arrays" in {
      val list1 = List(1, 2, 3, 4, 8, 9)
      val list2 = List(0, 0, 6, 5, 6, 7)
      IntegerUtil.commonElements(list1, list2).size shouldBe 0
    }

    "find the common elements of 2 int arrays alt impl" in {
      val list1 = List(1, 2, 3, 4, 8, 9)
      val list2 = List(0, 3, 6, 5, 4, 7)
      IntegerUtil.commonElementsAlt(list1, list2).size shouldBe 2
    }

    "find zero common elements of 2 int arrays alt impl" in {
      val list1 = List(1, 2, 3, 4, 8, 9)
      val list2 = List(0, 0, 6, 5, 6, 7)
      IntegerUtil.commonElementsAlt(list1, list2).size shouldBe 0
    }

    "find 10 fibonacci sequence numbers using foldLeft" in {
      IntegerUtil.fibonacciSeq(10) shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    }

    "find fibonacci number for 35 using foldLeft" in {
      IntegerUtil.fibonacciNum(0) shouldBe 0
      IntegerUtil.fibonacciNum(1) shouldBe 1
      IntegerUtil.fibonacciNum(2) shouldBe 1
      IntegerUtil.fibonacciNum(3) shouldBe 2
      IntegerUtil.fibonacciNum(35) shouldBe 9227465
    }

    "find fibonacci number for 35 using tail recursion" in {
      IntegerUtil.fibonacciNumTailRec(0) shouldBe 0
      IntegerUtil.fibonacciNumTailRec(1) shouldBe 1
      IntegerUtil.fibonacciNumTailRec(2) shouldBe 1
      IntegerUtil.fibonacciNumTailRec(3) shouldBe 2
      IntegerUtil.fibonacciNumTailRec(35) shouldBe 9227465
    }

    "print out the binary form of an int" in {
      IntegerUtil.toBinary(156) shouldBe "10011100"
      IntegerUtil.toBinary(1) shouldBe "1"
      IntegerUtil.toBinary(2) shouldBe "10"
      IntegerUtil.toBinary(3) shouldBe "11"
      IntegerUtil.toBinary(4) shouldBe "100"
    }

    "parse a String representation of an integer to Int" in {
      IntegerUtil.parseInt("1234") shouldBe 1234
    }

    "parse a String representation of an integer to Int using tail recursion" in {
      IntegerUtil.parseIntTailRec("1234") shouldBe 1234
    }
  }

}
