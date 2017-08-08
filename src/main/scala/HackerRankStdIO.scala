object HackerRankStdIO {

  def main(args: Array[String])(): Unit = {
    val sumResult = io.Source.stdin.getLines().take(2).map(_.toInt).sum
    println(sumResult)
  }

}
