import Util.readFile

import scala.annotation.tailrec

@main def day10(): Unit = {
  val input = readFile("resources/day10")

  val syntaxCheckerScores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val autocompleteScores = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  val brackets = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  @tailrec
  def findIllegalCharacter(s: String, stack: List[Char] = List.empty): Option[Char] = {
    if (s.isEmpty) None
    else if (brackets.keySet.contains(s.head)) findIllegalCharacter(s.tail, stack.prepended(s.head))
    else if (brackets.get(stack.head).contains(s.head)) findIllegalCharacter(s.tail, stack.tail)
    else Some(s.head)
  }

  @tailrec
  def findUnbalanced(s: String, stack: List[Char] = List.empty): Option[List[Char]] = {
    if (s.isEmpty) Some(stack)
    else if (brackets.keySet.contains(s.head)) findUnbalanced(s.tail, stack.prepended(s.head))
    else if (brackets.get(stack.head).contains(s.head)) findUnbalanced(s.tail, stack.tail)
    else None
  }

  println(input.foldLeft(0) { (acc, s) =>
    acc + findIllegalCharacter(s).map(syntaxCheckerScores(_)).getOrElse(0)
  })

  val xs = input.flatMap { s =>
    findUnbalanced(s).map(_.foldLeft(BigInt(0)) { (acc, ch) => acc * 5 + autocompleteScores(ch) })
  }.sorted

  println(xs(xs.length / 2))
}