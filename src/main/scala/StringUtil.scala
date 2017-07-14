import scala.annotation.tailrec

object StringUtil {

  def nonRepeating(input: String): Option[Char] = {
    val uniques = input.groupBy(c => c).mapValues(_.size).filter(e => e._2 == 1)

    input.foldLeft(None: Option[Char]) {
      (acc, next) =>
        if (acc.equals(None) && uniques.contains(next))
          Some(next)
        else
          acc
    }
  }

  def reverse(input: String): String = {
    if (input.isEmpty) ""
    else reverse(input.tail) + input.head
  }

  def reverseNested(input: String): String = {
    @tailrec
    def inner(in: String, acc: String = ""): String = {
      if (in.isEmpty) acc
      else inner(in.tail, in.head + acc)
    }
    inner(input)
  }

  def reverseFold(input: String): String = {
    input.foldLeft(""){
      (rev, next) =>
        next + rev
    }
  }

}
