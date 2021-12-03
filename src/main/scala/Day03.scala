import Util.{getBit, readFile, setBit}

import scala.annotation.tailrec
import scala.io.Source

def calculateBitSum(sum: Int, x: Int, position: Int): Int = sum + getBit(x, position)

def calculateBitSums(sums: List[Int], x: Int): List[Int] = sums.zipWithIndex.map { (y, i) => calculateBitSum(y, x, i) }

@tailrec
def findValue(xs: List[Int], position: Int, coefficient: Boolean): Int = {
  if (xs.length == 1) xs.head
  else {
    val bitSum = xs.foldLeft(0) { (acc, x) => calculateBitSum(acc, x, position) }
    val criterion = if ((bitSum >= (xs.length.toDouble / 2).ceil) ^ coefficient) 1 else 0
    findValue(
      xs.filter(getBit(_, position) == criterion),
      position - 1,
      coefficient
    )
  }
}

@main def day03(): Unit = {
  val input = readFile("resources/day03")
  val inputInt = input.map(Integer.parseInt(_, 2))

  val bitStringLength = input.head.length()

  val (gamma, epsilon) = inputInt
    .foldLeft(List.fill(bitStringLength)(0))(calculateBitSums)
    .zipWithIndex
    .foldLeft((0, 0)) { (acc, element) =>
      val (g, e) = acc
      val (x, i) = element
      if (x > input.length / 2) (setBit(g, i), e)
      else (g, setBit(e, i))
    }

  println(gamma * epsilon)

  val oxygenGeneratorRating = findValue(inputInt, bitStringLength - 1, true)
  val co2ScrubberRating = findValue(inputInt, bitStringLength - 1, false)

  println(oxygenGeneratorRating * co2ScrubberRating)
}