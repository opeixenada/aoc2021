import Util.readFile

import scala.annotation.tailrec

@main def day14(): Unit = {
  val input = readFile("resources/day14")

  val template = input.head

  val rules = input.drop(2).map { x =>
    val split = x.split(" -> ")
    split.head -> split.last
  }.toMap

  var state: Map[(String, Int), Map[Char, BigInt]] = Map.empty

  def getOccurrences(s: String): Map[Char, BigInt] = s.groupBy(identity).map((k, v) => k -> BigInt(v.length))

  def calculateOccurrences(s: String, times: Int): Map[Char, BigInt] =
    if (times == 0) getOccurrences(s.tail)
    else
      state.getOrElse((s, times),
        {
          val middle = rules(s)
          val left = calculateOccurrences(s"${s.head}$middle", times - 1)
          val right = calculateOccurrences(s"$middle${s.tail}", times - 1)
          val occurrences = left.toList
            .appendedAll(right.toList)
            .groupBy(_._1).map {
            (k, v) => k -> v.map(_._2).sum
          }
          state = state +
            ((s -> times) -> occurrences) +
            ((s"${s.head}$middle" -> (times - 1)) -> left) +
            ((s"$middle${s.tail}" -> (times - 1)) -> right)
          occurrences
        }
      )

  def occurrencesDifference(times: Int) = {
    val occurrences = template.zip(template.tail).map { case (a, b) =>
      calculateOccurrences(s"$a$b", times)
    }.flatMap(_.toList).appended(template.head -> BigInt(1)).groupBy(_._1).map {
      (k, v) => k -> v.map(_._2).sum
    }

    occurrences.values.max - occurrences.values.min
  }

  println(occurrencesDifference(10))
  println(occurrencesDifference(40))
}