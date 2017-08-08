import scala.annotation.tailrec

object AssortedUtil {

  def largestElementsDecreasing(input: List[Int], num: Int): List[Int] =
    input.sortWith(_ > _)
      .take(num)

  def reverseLinkedListGroups(input: List[Int], groupSize: Int): List[Int] =
    input.sliding(groupSize, groupSize)
      .map(_.reverse)
      .flatten
      .toList

  def addLinkedListNumbers(listA: List[Int], listB: List[Int]): List[Int] = {
    val res = listA.reverse.mkString.toInt + listB.reverse.mkString.toInt
    res.toString.reverse.map(_.asDigit).toList
  }

  def nextLargerElement(vec: Vector[Int]): Vector[Int] = {
    @tailrec
    def inner(remaining: Vector[Int], res: Vector[Int] = Vector.empty[Int]): Vector[Int] = remaining match {
      case IndexedSeq() => res
      case head +: tail =>
        val nextLarger = tail.find(_ > head).getOrElse(-1)
        inner(tail, res :+ nextLarger)
    }
    inner(vec)
  }

}
