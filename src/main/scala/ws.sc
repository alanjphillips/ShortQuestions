import scala.collection.immutable.ListMap
import scala.util.Try

val input = "axbxaabaacaaaa"

input.groupBy(e => e)

input.toList

input.flatMap(c => c.toUpper.toString)

input.flatten(c => c.toString.toUpperCase).zipWithIndex.map(e => e.swap).toMap

input.toCharArray.zipWithIndex.map(e => e.swap).toMap

val orderedCharCounts: Map[Char, Int] = input.foldLeft(Map.empty[Char, List[Char]]){
  (acc, nextVal) =>
    val lst = acc.get(nextVal).getOrElse(List.empty[Char])
    acc + (nextVal -> lst.::(nextVal))
}.mapValues(_.size)

input.foldLeft(ListMap.empty[Char, List[Char]]) {
  (acc, nextVal) =>
    val lst = acc.get(nextVal).getOrElse(List.empty[Char])
    acc + (nextVal -> lst.::(nextVal))
}.mapValues(_.size)   //Map[Char, Int]
  .filter(e => e._2 == 1)
  .headOption.map(res => res._1)

Try("12.8".toDouble).toOption


List(0, 1).takeRight(0)

val foo = List(None, None, Some(1), None, Some(2))
foo.flatten