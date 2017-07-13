
object StringUtil {

  def nonRepeating(input: String) : Option[Char] = {
    val uniques = input.groupBy(c => c).mapValues(_.size).filter(e => e._2 == 1)

    input.foldLeft(None: Option[Char]) {
      (acc, next) =>
        if (acc.equals(None) && uniques.contains(next))
          Some(next)
        else
          acc
    }
  }



}
