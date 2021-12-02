import Util.readFile

import scala.io.Source

def countIncreases(xs: List[Int]): Int = xs.zip(xs.tail).count(_ < _)

def countSlidingSumIncreases(xs: List[Int]): Int =
  countIncreases(xs.zip(xs.tail).zip(xs.tail.tail).map { (x, y) => x._1 + x._2 + y })

def countSlidingSumIncreasesFold(xs: List[Int]): Int = xs.foldLeft((
  0,
  xs.take(3).sum,
  xs.tail.take(3).sum,
  xs.slice(2, 4).sum,
  xs.drop(3).head
)) { (acc, x) =>
  val (count, a, b, c, d) = acc
  (count + (if (a < b) 1 else 0), b, c + x, d + x, x)
}._1

@main def day01(): Unit = {
  val input = readFile("resources/day01").map(_.toInt)
  println(countIncreases(input))
  println(countSlidingSumIncreases(input))
  println(countSlidingSumIncreasesFold(input))
}
