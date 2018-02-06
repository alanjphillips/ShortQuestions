import scala.annotation.tailrec

/**
  * http://codingbat.com/java/Recursion-1
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

  def factorial(n: Int): Int = {
    @tailrec
    def factorialAcc(n: Int, acc: Int = 1): Int = n match {
      case 1 => acc
      case _ => factorialAcc(n - 1, acc * n)
    }

    factorialAcc(n)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def fibonacciAcc(n: Int, prevprev: Int = 0, prev: Int = 1): Int = n match {
      case 0 => prevprev
      case 1 => prev
      case _ => fibonacciAcc(n - 1, prev, prev + prevprev)
    }

    fibonacciAcc(n)
  }

  def fibonacciFoldLeft(n: Int): Int = {
    (2 to n).foldLeft(Seq[Int](0, 1)){
      (acc, num) => acc :+ (acc.last + acc(num - 2))
    }(n)
  }

  def reverse(str: String): String = {
    @tailrec
    def reverseAcc(str: String, acc: String = ""): String = str match {
      case "" => acc
      case s  => reverseAcc(s.tail, s.head + acc)
    }

    reverseAcc(str)
  }

}
