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

  }

}