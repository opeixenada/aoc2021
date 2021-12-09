import Util.readFile

import scala.annotation.tailrec
import Math.*

type Coordinates = (Int, Int)
type Basin = Set[Coordinates]

@main def day09(): Unit = {
  val input: IndexedSeq[IndexedSeq[Int]] = readFile("resources/day09").map(_.map(_.toString.toInt)).toIndexedSeq

  def getNeighbours(i: Int, j: Int): Seq[Coordinates] =
    (for (x <- max(0, i - 1) to min(i + 1, input.length - 1);
          y <- max(0, j - 1) to min(j + 1, input.head.length - 1))
    yield (x, y))
      .filterNot((x, y) => x == i && y == j)

  val minimums = (for (i <- input.indices; j <- input.head.indices) yield {
    val isMinimum: Boolean = getNeighbours(i, j)
      .map(input(_)(_))
      .foldLeft(true) { (acc, x) => acc && input(i)(j) < x }

    if (isMinimum) Seq(i -> j) else Seq.empty
  }).flatten

  println(minimums.foldLeft(0) { case (acc, (x, y)) => acc + input(x)(y) + 1 })

  @tailrec
  def findBasins(incompleteBasins: List[Basin], completeBasins: List[Basin]): List[Basin] = incompleteBasins match {
    case x :: xs =>
      val newBasinPoints = x.flatMap { (i, j) =>
        getNeighbours(i, j).filter { (a, b) =>
          (i == a || j == b) && !x.contains(a -> b) && input(a)(b) != 9
        }
      }

      if (newBasinPoints.isEmpty) findBasins(xs, completeBasins.+:(x ++ newBasinPoints))
      else findBasins(xs.+:(x ++ newBasinPoints), completeBasins)

    case _ => completeBasins
  }

  println(findBasins(minimums.map(Set(_)).toList, Nil).map(_.size).sorted.reverse.take(3).product)
}