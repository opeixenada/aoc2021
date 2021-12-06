import Util.readFile

import scala.io.Source

@main def day01(): Unit = {
  val input = readFile("resources/day01").map(_.toInt)

  def countIncreases(shift: Int): Int = input.zip(input.drop(shift)).count(_ < _)

  println(countIncreases(1))
  println(countIncreases(3))
}
