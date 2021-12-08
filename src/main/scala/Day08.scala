import Util.readFile

import scala.annotation.tailrec

@main def day08(): Unit = {
  val input = readFile("resources/day08")

  val sum1 = input.map(_.split('|').last.split(' ').count(x => Set(2, 3, 4, 7).contains(x.length))).sum

  println(sum1)

  val digitsMapping = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "abdfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9
  ).map((k, v) => k.toSet -> v)

  def getMapping(foo: Char => (Int, Char)): Map[Int, Set[Char]] =
    "abcdefg".map(foo).groupBy(_._1).map((key, xs) => key -> xs.map(_._2).toSet)

  def getFrequencies(xs: Iterable[Set[Char]]): Map[Int, Set[Char]] = getMapping { char =>
    xs.count(_.contains(char)) -> char
  }

  def getLengths(xs: Iterable[Set[Char]]): Map[Int, Set[Char]] = getMapping { char =>
    xs.filter(_.contains(char)).minBy(_.size).size -> char
  }

  @tailrec
  def findMapping(determinedMapping: Map[Char, Char],
                  workingMapping: Map[Char, Set[Char]],
                  digits: Seq[Set[Char]],
                  patterns: Seq[Set[Char]]): Map[Char, Char] =
    if (workingMapping.isEmpty) determinedMapping
    else {
      def getMapping(foo: Iterable[Set[Char]] => Map[Int, Set[Char]]) = foo(patterns).flatMap { (x, s) =>
        s.map(_ -> foo(digits)(x))
      }

      val updatedMapping = workingMapping.map { (k, v) =>
        k -> v.intersect(getMapping(getFrequencies)(k)).intersect(getMapping(getLengths)(k))
      }

      val newDeterminedMapping = determinedMapping ++ updatedMapping.filter(_._2.size == 1).map((k, v) => k -> v.head)
      val newWorkingMapping = updatedMapping.filter(_._2.size > 1)
      val newDigits = digits.map(_.diff(newDeterminedMapping.keySet))
      val newPatterns = patterns.map(_.diff(newDeterminedMapping.keySet))

      findMapping(newDeterminedMapping, newWorkingMapping, newDigits, newPatterns)
    }

  val sum2: Int = input.map { s =>
    val split = s.split('|')

    val signalMapping = findMapping(
      Map.empty,
      "abcdefg".map(_ -> "abcdefg".toSet).toMap,
      digitsMapping.keys.toSeq,
      split.head.split(' ').map(_.toSet)
    )

    split.last.trim.split(' ').map { s =>
      digitsMapping(s.map(signalMapping).toSet)
    }.mkString.toInt
  }.sum

  println(sum2)
}