import scala.collection.immutable.Queue
import scala.math.BigDecimal.RoundingMode

object HackerRankWarmUp {

  def simpleArraySum(n: Int, ar: Array[Int]): Int =  {
    ar.sum
  }

  def compareTriplets(tripletA: List[Int], tripletB: List[Int]): (Int, Int) = {
    val comb = tripletA zip tripletB
    comb.foldLeft((0, 0)) {
      (acc, next) => {
          if (next._1 > next._2)
            (acc._1 + 1, acc._2)
          else if (next._2 > next._1)
            (acc._1, acc._2+ 1)
          else
            acc
      }
    }
  }

  def aVeryBigSum(n: Int, ar: Array[Long]): Long =  {
    ar.sum
  }

  def diagonalDifference(matrix: Array[Array[Int]]): Int = {
    val primarySum = matrix.foldLeft((Queue.empty[Int], 0)) {
      (acc, nextArr) => (acc._1 :+ nextArr(acc._2), acc._2 + 1)
    }._1.sum

    val secondarySum = matrix.foldRight((Queue.empty[Int], 0)) {
      (nextArr, acc) => (acc._1 :+ nextArr(acc._2), acc._2 + 1)
    }._1.sum

    math.abs(primarySum - secondarySum)
  }

  def fractionOfArray(arr: Array[Int]): Seq[Double] = {
    val grps = arr.groupBy[Int](_.compare(0))
      .mapValues(v =>
        BigDecimal(v.length.toDouble / arr.length.toDouble)
          .setScale(6, RoundingMode.HALF_UP)
          .toDouble
      )

    List(
      grps.getOrElse(1, 0),
      grps.getOrElse(-1, 0),
      grps.getOrElse(0, 0)
    )
  }

}
