import Util.{Coordinates, intMatrix}

import scala.annotation.tailrec

@main def day15(): Unit = {
  val input = intMatrix("resources/day15")

  def getValue(x: Int, y: Int): Int = {
    val a = x / input.length
    val b = y / input.length
    val c = input(x - a * input.length)(y - b * input.length) + a + b
    if (c > 9) c % 10 + 1 else c
  }

  @tailrec
  def findMinRisk(
                   currentCoordinates: Coordinates,
                   currentValue: Int,
                   visited: Set[Coordinates],
                   calculated: Map[Coordinates, Int],
                   currentMinRisk: Int,
                   width: Int,
                 ): Int = {
    val newCalculated = List(
      (currentCoordinates._1 + 1) -> currentCoordinates._2,
      (currentCoordinates._1 - 1) -> currentCoordinates._2,
      currentCoordinates._1 -> (currentCoordinates._2 - 1),
      currentCoordinates._1 -> (currentCoordinates._2 + 1)
    )
      .filterNot(visited.contains)
      .filter { case (x, y) => x > -1 && x < width && y > -1 && y < width }
      .foldLeft(calculated) { case (acc, (x, y)) =>
        acc + ((x -> y) -> Math.min(acc.getOrElse(x -> y, Int.MaxValue), currentValue + getValue(x, y)))
      }

    if (newCalculated.isEmpty) currentMinRisk
    else {
      val newNode = newCalculated.minBy(_._2)._1

      val minRisk =
        if (newNode._1 == width - 1 && newNode._2 == width - 1) Math.min(currentMinRisk, newCalculated(newNode))
        else currentMinRisk

      findMinRisk(
        newNode,
        newCalculated(newNode),
        visited + currentCoordinates,
        newCalculated.filterNot(_._1 == newNode),
        minRisk,
        width,
      )
    }
  }

  def calculateMinRisk(multiplier: Int = 1) = findMinRisk(
    0 -> 0,
    0,
    Set.empty,
    Map.empty,
    Int.MaxValue,
    input.length * multiplier,
  )

  println(calculateMinRisk())
  println(calculateMinRisk(5))
}