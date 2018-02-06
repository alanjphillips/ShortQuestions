import org.scalatest.{Matchers, WordSpec}

import Recursion._

class RecursionSpec extends WordSpec with Matchers {

  "Recursion" should {

    "sum" in {
      sum(List(1, 2, 3)) shouldBe 6
    }

    "sum tail recursive" in {
      sumTailRec(List(1, 2, 3)) shouldBe 6
    }

    "product" in {
      product(2, 2) shouldBe 4
      product(2 ,1) shouldBe 2
    }

    "power" in {
      power(2, 3) shouldBe 8
      power(2, 1) shouldBe 2
      power(2, 0) shouldBe 1
    }

    "factorial" in {
      factorial(1) shouldBe 1
      factorial(2) shouldBe 2
      factorial(3) shouldBe 6
    }

    "fibonacci" in {
      fibonacci(0) shouldBe 0
      fibonacci(1) shouldBe 1
      fibonacci(2) shouldBe 1
      fibonacci(3) shouldBe 2
      fibonacci(4) shouldBe 3
      fibonacci(5) shouldBe 5
      fibonacci(8) shouldBe 21
    }

    "fibonacciFoldLeft" in {
      fibonacciFoldLeft(0) shouldBe 0
      fibonacciFoldLeft(1) shouldBe 1
      fibonacciFoldLeft(2) shouldBe 1
      fibonacciFoldLeft(3) shouldBe 2
      fibonacciFoldLeft(4) shouldBe 3
      fibonacciFoldLeft(5) shouldBe 5
      fibonacciFoldLeft(8) shouldBe 21
    }

  }

}