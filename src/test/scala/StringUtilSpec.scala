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

    "reverse a String recursively" in {
      val input = "abcdef"
      StringUtil.reverse(input) shouldBe "fedcba"
    }

    "reverse a String recursively nested function" in {
      val input = "abcdef"
      StringUtil.reverseNested(input) shouldBe "fedcba"
    }

    "reverse a String using fold" in {
      val input = "abcdef"
      StringUtil.reverseFold(input) shouldBe "fedcba"
    }

    "determine that 2 Strings are anagrams" in {
      val str1 = "listen"
      val str2 = "silent"
      StringUtil.anagram(str1, str2) shouldBe true
    }

    "determine that 2 Strings are not anagrams" in {
      val str1 = "listen"
      val str2 = "silence"
      StringUtil.anagram(str1, str2) shouldBe false
    }

    "check that String is a palindrome" in {
      val word = "kayak"
      StringUtil.palindrome(word) shouldBe true
    }

    "check that String is not a palindrome" in {
      val word = "canoe"
      StringUtil.palindrome(word) shouldBe false
    }

  }

}
