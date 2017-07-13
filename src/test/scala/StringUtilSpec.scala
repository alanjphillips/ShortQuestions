import org.scalatest.{Matchers, WordSpec}

class StringUtilSpec extends WordSpec with Matchers {

  "StringUtil" should {

    "find the first non-repeated character in a String" in {
      val input1 = "aaabaacacaaa"
      StringUtil.nonRepeating(input1) shouldBe Some('b')

      val input2 = "aaabbaacaaaa"
      StringUtil.nonRepeating(input2) shouldBe Some('c')

      val input3 = "axaabaacaaaa"
      StringUtil.nonRepeating(input3) shouldBe Some('x')

      val input4 = "aaabbaacaacaa"
      StringUtil.nonRepeating(input4) shouldBe None
    }

  }

}
