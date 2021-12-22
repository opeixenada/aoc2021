import Util.readFile

import java.util.UUID
import scala.annotation.tailrec

@main def day18(): Unit = {

  case class Leaf(index: Int, depth: Int, value: Long)

  sealed trait Element {
    def add(other: Element): Element = Pair(this, other).reduce

    def magnitude: Long

    private def getDepths(x: Element, depth: Int): List[(Int, Long)] = x match
      case Pair(l, r) => getDepths(l, depth + 1) ++ getDepths(r, depth + 1)
      case Number(v) => List(depth - 1 -> v)

    def toLeaves: List[Leaf] =
      getDepths(this, 0).zipWithIndex.map { case ((d, v), i) => Leaf(i, d, v) }

    private def explode: Option[Element] =
      toLeaves.filter(_._2 == 4).take(2) match
        case x :: y :: Nil => Some(explode(x.index - 1, x.value, y.index + 1, y.value))
        case _ => None

    private def explode(leftIndex: Int, leftValue: Long, rightIndex: Int, rightValue: Long): Element = {
      def update(i: Int, x: Element): (Int, Element) = x match
        case Pair(_: Number, _: Number) if i == leftIndex + 1 => (i + 2) -> Number(0)
        case Pair(l, r) =>
          val (lastIndexLeft, elementLeft) = update(i, l)
          val (lastIndexRight, elementRight) = update(lastIndexLeft, r)
          (lastIndexRight, Pair(elementLeft, elementRight))
        case Number(v) if i == leftIndex => (i + 1) -> Number(v + leftValue)
        case Number(v) if i == rightIndex => (i + 1) -> Number(v + rightValue)
        case n => (i + 1) -> n

      update(0, this)._2
    }

    private def split: Option[Element] = this match
      case Number(v) if v >= 10 => Some(Pair(Number(v / 2), Number(v / 2 + v % 2)))
      case _: Number => None
      case Pair(l, r) => (l.split, r.split) match
        case (Some(s), _) => Some(Pair(s, r))
        case (None, Some(s)) => Some(Pair(l, s))
        case _ => None

    @tailrec
    final def reduce: Element = explode.orElse(split) match
      case None => this
      case Some(x) => x.reduce
  }

  case class Number(value: Long) extends Element {
    def magnitude: Long = value
  }

  case class Pair(left: Element, right: Element) extends Element {
    def magnitude: Long = left.magnitude * 3 + right.magnitude * 2
  }

  @tailrec
  def toRPN(s: String, operators: List[Char] = List.empty, output: List[String] = List.empty): List[String] =
    if (s.isEmpty) output
    else
      (s.head, operators) match
        case (',', o :: os) if o != '[' => toRPN(s, os, o.toString :: output)
        case (',', _) => toRPN(s.tail, ',' :: operators, output)
        case ('[', _) => toRPN(s.tail, '[' :: operators, output)
        case (']', o :: os) if o != '[' => toRPN(s, os, o.toString :: output)
        case (']', o :: os) => toRPN(s.tail, os, output)
        case _ =>
          val digits = s.takeWhile(_.isDigit)
          toRPN(s.drop(digits.length), operators, digits :: output)

  def parseElement(tokens: List[String]): (Element, List[String]) =
    tokens.head match
      case "," =>
        val (children, rest) = (0 until 2).foldLeft((List.empty[Element], tokens.tail)) {
          case ((es, ts), _) =>
            val (e, rest) = parseElement(ts)
            (e :: es, rest)
        }
        (Pair(children.head, children.last), rest)
      case x => (Number(x.toInt), tokens.tail)

  val input: List[Element] = readFile("resources/day18").map(s => parseElement(toRPN(s))._1)

  println(input.reduce(_.add(_)).magnitude)

  println((for {
    x <- input
    y <- input.filterNot(_ == x)
  } yield {
    x.add(y).magnitude
  }).max)
}
