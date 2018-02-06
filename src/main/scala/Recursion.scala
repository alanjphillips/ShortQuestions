import scala.annotation.tailrec

/**
  * http://codingbat.com/java/Recursion-1
  *
  * A list of recursion problems here to try out (in Scala instead of Java tho)
  *
  */
object Recursion {

  def sum(numbers: List[Int]): Int = numbers match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }

  def sumTailRec(numbers: List[Int]): Int = {
    @tailrec
    def sum(numbers: List[Int], acc: Int = 0): Int = numbers match {
      case Nil => acc
      case head :: tail => sum(tail, acc + head)
    }

    sum(numbers)
  }


  def product(a: Int, b: Int): Int = {
    @tailrec
    def productAcc(a: Int, count: Int, acc: Int = 0): Int = count match {
      case 0 => acc
      case _ => productAcc(a, count - 1, acc + a)
    }

    productAcc(a, b)
  }


  def power(base: Int, exp: Int): Int = {
    @tailrec
    def powerAcc(base: Int, exp: Int, acc: Int = 1): Int = exp match {
      case 0 => acc
      case _ => powerAcc(base, exp - 1, acc * base)

    }

    powerAcc(base, exp)
  }



}
