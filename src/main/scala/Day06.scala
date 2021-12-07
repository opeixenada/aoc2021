import Util.intList

@main def day06(): Unit = {
  val input = intList("resources/day06")

  var cache: Map[Int, BigInt] = Map.empty

  def f(n: Int): BigInt = {
    val x: BigInt = cache.getOrElse(n, {
      if (n < 1) 1 else f(n - 7) + f(n - 9)
    })
    cache = cache + (n -> x)
    x
  }

  def population(afterDays: Int) = input.map { x => f(afterDays - x) }.sum

  println(population(80))
  println(population(256))
}