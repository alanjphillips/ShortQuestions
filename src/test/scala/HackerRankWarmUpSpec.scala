import org.scalatest.{Matchers, WordSpec}

import HackerRankWarmUp._

class HackerRankWarmUpSpec extends WordSpec with Matchers {

  "Warm up" should {

    "sum array" in {
      simpleArraySum(6, Array(1, 2, 3, 4, 10, 11)) shouldBe 31
    }

    "compare the triplets" in {
      compareTriplets(List(5, 6, 7), List(3, 6, 10)) shouldBe (1, 1)
    }

    "sum array of big numbers" in {
      aVeryBigSum(5, Array[Long](1000000001L, 1000000002L, 1000000003L, 1000000004L, 1000000005L)) shouldBe 5000000015L
    }

    "calculate the absolute difference between the sums of matrix diagonals" in {
      val matrix = Array(
        Array(11, 2, 4),
        Array(4, 5, 6),
        Array(10, 8, -12)
      )

      diagonalDifference(matrix) shouldBe 15
    }

    "calculate fraction of array for positive, negative, and zero" in {
      fractionOfArray(Array(-4, 3, -9, 0, 4, 1)) shouldBe Seq[Double](0.500000, 0.333333, 0.166667)
    }

  }

}
