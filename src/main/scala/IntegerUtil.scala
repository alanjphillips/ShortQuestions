import scala.annotation.tailrec

object IntegerUtil {

  def frequentInteger(arr: Array[Int]): Int =
    arr.groupBy(e => e)
      .mapValues(_.size)
      .reduceLeft(
        (a, b) => if (a._2 > b._2) a else b
      )._1

  def frequentInteger(lst: List[Int]): Int =
    lst.groupBy(e => e)
      .mapValues(_.size)
      .reduceLeft(
        (a, b) => if (a._2 > b._2) a else b
      )._1

  def findPairsForDistinct(lst: List[Int], sum: Int): Map[Int, Int] = {
    def pairs(lst: List[Int], acc: List[(Int, Int)] = Nil): List[(Int, Int)] = lst match {
      case Nil          => acc
      case _ :: Nil     => acc
      case head :: tail =>
        val r = lst.foldLeft(List[(Int, Int)]()) {
          (a, b) =>
            if (head != b && head + b == sum)
              (head, b) :: a
            else
              a
        }
        pairs(tail, acc ::: r)
    }
    pairs(lst.distinct).toMap
  }

  def isRotated(original: List[Int], subject: List[Int]): Boolean =
    original.size == subject.size && (subject ::: subject).containsSlice(original)

  def uniqueElement(lst: List[Int]): Option[Int] = {
    val uniques = lst.groupBy(e => e)
      .mapValues(_.size)
      .filter(a => a._2 == 1)

    if (uniques.size == 1)
      uniques.headOption.map(result => result._1)
    else
      None
  }

  def commonElements(list1: List[Int], list2: List[Int]): List[Int] =
    list1.intersect(list2)

  def commonElementsAlt(list1: List[Int], list2: List[Int]): List[Int] = {
    val lookup1 = list1.map(e => (e,e)).toMap
    list2.filter(e => lookup1.contains(e))
  }

  def fibonacciSeq(seqSize: Int): List[Long] =
    (0 to seqSize - 2).foldLeft(List[Long](0,1)) {
      (acc, _) => acc :+ (acc.last + acc(acc.size-2))
    }

  def fibonacciNum(num: Int): Long =
    (0 to num - 2).foldLeft(List[Long](0,1)) {
      (acc, _) => acc :+ (acc.last + acc(acc.size-2))
    }(num)

  def fibonacciNumTailRec(num: Int): Long = {
    @tailrec
    def innerFib(num: Int, secondLast: Int = 0, last: Int = 1): Int = num match {
      case 1 => last
      case 0 => secondLast
      case _ => innerFib(num - 1, last, secondLast + last)
    }
    innerFib(num)
  }

  def toBinary(intNumber: Int): String = {
    @tailrec
    def divider(intNumber: Int, binary: String = ""): String = {
      if (intNumber / 2 == 0)
        (intNumber % 2).toString + binary
      else
        divider(intNumber / 2, (intNumber % 2).toString + binary)
    }
    divider(intNumber)
  }

  def parseInt(strInteger: String): Int =
    strInteger.foldLeft(0) {
      (acc, next) => (acc * 10) + (next - 48)
    }

  def parseIntTailRec(strInteger: String): Int = {
    @tailrec
    def parse(next: String, acc: Int = 0): Int = {
      if (next.isEmpty)
        acc
      else {
        val res = (acc * 10) + (next.head - 48)
        parse(next.tail, res)
      }
    }
    parse(strInteger)
  }

}
