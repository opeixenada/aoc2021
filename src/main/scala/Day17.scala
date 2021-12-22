import Util.readFile

import scala.annotation.tailrec

@main def day17(): Unit = {
  def getRange(s: String): Range.Inclusive = {
    val xs = s.split("\\.\\.").map(_.toInt)
    xs.head to xs.last
  }

  val input = readFile("resources/day17").head.stripPrefix("target area: x=").split(", y=").map(getRange)

  val xs = input.head
  val ys = input.last

  @tailrec
  def checkVelocity(x: Int, y: Int, a: Int = 0, b: Int = 0, maxY: Int = 0): Option[Int] = {
    if (xs.contains(a) && ys.contains(b)) Some(maxY)
    else if ((a > xs.last && x >= 0) || (a < xs.head && x == 0) || (b < ys.head && y <= 0)) None
    else checkVelocity(if (x > 0) x - 1 else 0, y - 1, a + x, b + y, Math.max(maxY, b))
  }

  @tailrec
  def checkHorizontalVelocity(x: Int, a: Int = 0): Boolean =
    if (xs.contains(a)) true
    else if ((a > xs.last && x >= 0) || (a < xs.head && x == 0)) false
    else checkHorizontalVelocity(if (x > 0) x - 1 else 0, a + x)

  @tailrec
  def findMaxHeights(x: Int, y: Int = ys.min, hs: List[Int] = List.empty): List[Int] =
    (checkVelocity(x, y), hs) match
      case (Some(h), as) => findMaxHeights(x, y + 1, h :: as)
      case (None, as) if y < 100 => findMaxHeights(x, y + 1, as)
      case _ => hs

  val maxHeights = (Math.min(1, xs.head) to Math.max(0, xs.last))
    .filter(checkHorizontalVelocity(_))
    .flatMap(findMaxHeights(_))

  println(maxHeights.max)
  println(maxHeights.size)
}
