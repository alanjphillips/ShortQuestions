import scala.collection.immutable.Queue
import scala.math.BigDecimal.RoundingMode

object HackerRankWarmUp {

  def simpleArraySum(n: Int, ar: Array[Int]): Int = ar.sum

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

  def aVeryBigSum(n: Int, ar: Array[Long]): Long = ar.sum

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

  def staircase(n: Int): Seq[String] = {
    (1 to n).map(
      i => Vector.fill(n - i)(' ').mkString + Vector.fill(i)('#').mkString
    )
  }

  def minMaxSum(input: Array[Int]): (Long, Long) = {
    val nums = input.map(_.toLong)
    val sums = nums.indices
      .map(nums.patch(_, Nil, 1).sum)
      .sortWith(_ < _)

    (sums.head, sums.last)
  }

  def birthdayCakeCandles(n: Int, ar: Array[Int]): Int = {
    ar.groupBy(c => c)
      .mapValues(_.length)
      .toVector
      .sortWith(_._1 > _._1)
      .headOption.getOrElse((0,0))._2
  }

  def timeConversion(s: String): String =  {
    val orig: Array[String] = s.split(":")
    val end =  ":" + orig.tail.mkString(":").dropRight(2)
    val hour = orig.head.toInt
    val start =
      if (orig.last.endsWith("PM") && hour != 12) (orig.head.toInt + 12).toString
      else if (orig.last.endsWith("AM") && hour == 12) "00"
      else orig.head
    start + end
  }

  def gradeStudent(n: Int, grades: Array[Int]): Array[Int] = {
    grades.map(
      g =>
        if (g < 38) g
        else {
          val diff = 5 - g % 5
          if (diff < 3) g + diff else g
        }
    )
  }

  def applesOranges(s: Int, t: Int, a: Int, b: Int, m: Int ,n: Int, apples: Array[Int], oranges: Array[Int]): List[Int] = {
    val appleCount = apples.map(
      apple => if (apple + a >= s && apple + a <= t) 1 else 0
    ).sum

    val orangeCount = oranges.map(
      orange => if (orange + b >= s && orange + b <= t) 1 else 0
    ).sum

    List(appleCount, orangeCount)
  }

  def electionWinner(votes: Array[String]): String = {
    val orderedCandidates = votes.groupBy(vote => vote)
      .mapValues(_.length)                      // gives (candidate name -> vote count)
      .toVector                                 // Map is converted to Vector of tuples (candidate name -> vote count) so it can be sorted
      .sortWith(_._2 > _._2)                    // sorted by vote count

    orderedCandidates
      .takeWhile(_._2 >= orderedCandidates.head._2)     // get joint Winners
      .sortWith(_._1 > _._1)                            // Sort by name, last alphabetically first, only sorts joint winners
      .head._1                                          // Take the winning tuple, _.1 presents the candidate name
  }


  def counts(nums: Array[Int], maxes: Array[Int]): Array[Int] = {
    maxes.map(                     // map over maxes
      max => nums.count(_ <= max)  // take each max and count in nums
    )
  }

}
