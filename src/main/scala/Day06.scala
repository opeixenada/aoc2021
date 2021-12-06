import Util.readFile

@main def day06(): Unit = {
  val input = readFile("resources/day06").head.split(',').map(_.toInt)

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