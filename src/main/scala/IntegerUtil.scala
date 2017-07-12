object IntegerUtil {

  def frequentInteger(arr: Array[Int]): Int = {
    arr.groupBy(e => e)
      .mapValues(_.size)
      .reduceLeft(
        (a, b) => if (a._2 > b._2) a else b
      )._1
  }

  def frequentInteger(lst: List[Int]): Int = {
    lst.groupBy(e => e)
      .mapValues(_.size)
      .reduceLeft(
        (a, b) => if (a._2 > b._2) a else b
      )._1
  }

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

}
