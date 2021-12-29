import Util.readFile

import scala.annotation.tailrec

@main def day22(): Unit = {

  def overlap(r1: (Int, Int), r2: (Int, Int)): (Int, Int) = Math.max(r1._1, r2._1) -> Math.min(r1._2, r2._2)

  sealed trait Expression {
    def volume: Long
  }

  case object Empty extends Expression {
    def volume: Long = 0L
  }

  case class Cuboid(on: Boolean, ranges: IndexedSeq[(Int, Int)]) extends Expression {
    def volume: Long = ranges.foldLeft(1L)((acc, x) => acc * (x._1 to x._2).size)

    def overlaps(other: Cuboid): Boolean = ranges.indices.forall { i =>
      val r = overlap(ranges(i), other.ranges(i))
      r._1 <= r._2
    }

    def merge(other: Cuboid): Cuboid = Cuboid(false, ranges.indices.map { i => overlap(ranges(i), other.ranges(i)) })
  }

  case class Difference(left: Expression, right: Cuboid) extends Expression {
    def volume: Long = left.volume - Intersection(left, right).volume
  }

  case class Intersection(left: Expression, right: Cuboid) extends Expression {
    def volume: Long = left match
      case Empty => Empty.volume
      case c: Cuboid => c.merge(right).volume
      case Intersection(l, r) => Intersection(l, r.merge(right)).volume
      case Difference(l, r) => Intersection(l, right).volume - Intersection(Intersection(l, r), right).volume
  }

  val input: List[Cuboid] = readFile("resources/day22").map { s =>
    val command = s.takeWhile(_ != ' ')
    val ranges = s.drop(command.length + 1).split(",").map(_.drop(2)).map { range =>
      val limits = range.split("\\.\\.").map(_.toInt)
      limits.head -> limits.last
    }

    Cuboid(command == "on", ranges)
  }

  def buildExpression(cuboids: List[Cuboid]): Expression = cuboids match
    case c :: cs if c.on => cs.filter(_.overlaps(c)).foldLeft[Expression](c)((e, cuboid) => Difference(e, cuboid))
    case _ => Empty

  def countOn(cs: List[Cuboid]) = cs.tails.map(buildExpression(_).volume).sum

  println(countOn(input.map { cuboid =>
    cuboid.copy(ranges = cuboid.ranges.map { r =>
      Math.max(r._1, -50) -> Math.min(r._2, 50)
    })
  }))

  println(countOn(input))
}

