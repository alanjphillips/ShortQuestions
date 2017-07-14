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

  def anagram(str1: String, str2: String): Boolean =
    str1.sorted equals str2.sorted

  def palindrome(word: String): Boolean =
    word equals word.reverse

  def uniqueChars(word: String): Boolean =
    word.groupBy(c => c)
      .mapValues(_.size)
      .filter(r => r._2 > 1)
      .isEmpty
}
