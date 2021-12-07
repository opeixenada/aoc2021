import Util.intList

@main def day07(): Unit = {
  val input = intList("resources/day07")
  val maxValue = input.max

  def getCheapestCost(cost: (Int, Int) => Int) = (0 until maxValue).foldLeft(Int.MaxValue) { (currentMin, y) =>
    Math.min(input.map(cost(_, y)).sum, currentMin)
  }

  println(getCheapestCost { (x, y) => (x - y).abs })
  println(getCheapestCost { (x, y) => (1 + (x - y).abs) * (x - y).abs / 2 })
}