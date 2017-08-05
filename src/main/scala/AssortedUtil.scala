

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
    val res = (listA.reverse.mkString.toInt) + (listB.reverse.mkString.toInt)
    res.toString.reverse.map(_.asDigit).toList
  }

}
