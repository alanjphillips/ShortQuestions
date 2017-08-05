import scala.annotation.tailrec
import scala.util.Try

object StringUtil {

  def nonRepeating(input: String): Option[Char] = {
    val uniques = input.groupBy[Char](c => c).mapValues(_.size).filter(e => e._2 == 1)

    input.foldLeft(None: Option[Char]) {
      (acc, next) =>
        if (acc.equals(None) && uniques.contains(next))
          Some(next)
        else
          acc
    }
  }

  def reverse(input: String): String =
    if (input.isEmpty) ""
    else reverse(input.tail) + input.head

  def reverseNested(input: String): String = {
    @tailrec
    def inner(in: String, acc: String = ""): String = {
      if (in.isEmpty) acc
      else inner(in.tail, in.head + acc)
    }
    inner(input)
  }

  def reverseFold(input: String): String =
    input.foldLeft(""){
      (rev, next) =>
        next + rev
    }

  def frequentLetter(input: String): Char =
    input.groupBy[Char](c => c)
      .mapValues(_.size)
      .reduceLeft(
        (a, b) => if (a._2 >= b._2) a else b
      )._1

  def anagram(str1: String, str2: String): Boolean =
    str1.sorted equals str2.sorted

  def palindrome(word: String): Boolean =
    word equals word.reverse

  def uniqueChars(word: String): Boolean =
    word.groupBy[Char](c => c)
      .mapValues(_.size)
      .filter(r => r._2 > 1)
      .isEmpty

  def intOrDouble(str: String): Option[String] = {
    val intOption = Try(str.toInt).toOption
    if (intOption != None)
      intOption.map(i => i.getClass.toString)
    else
      Try(str.toDouble).toOption.map(d => d.getClass.toString)
  }

}
