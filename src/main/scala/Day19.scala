import Util.readFile

import scala.annotation.tailrec

@main def day19(): Unit = {

  case class Coordinates(x: Int, y: Int, z: Int) {
    def turnX: Coordinates = Coordinates(x, -z, y)

    def turnY: Coordinates = Coordinates(-z, y, x)

    def turnZ: Coordinates = Coordinates(y, -x, z)

    def shift(p: Coordinates): Coordinates = Coordinates(x + p.x, y + p.y, z + p.z)

    def distance(other: Coordinates): Coordinates = Coordinates(x - other.x, y - other.y, z - other.z)

    def manhattan(other: Coordinates): Int = {
      val d = distance(other)
      d.x.abs + d.y.abs + d.z.abs
    }
  }

  def allTurns(scanner: Set[Coordinates]): List[Set[Coordinates]] =
    (0 to 2).foldLeft(List(scanner)) { (as, _) => as.head.map(_.turnX) :: as }.flatMap { x =>
      (0 to 2).foldLeft(List(x)) { (bs, _) => bs.head.map(_.turnY) :: bs }.flatMap { y =>
        (0 to 2).foldLeft(List(y)) { (cs, _) => cs.head.map(_.turnZ) :: cs }
      }
    }.distinct

  def relativeDistances(scanner: Set[Coordinates]): Map[Coordinates, Set[Coordinates]] =
    scanner.map { a =>
      a -> scanner.map { b =>
        Coordinates(a.x - b.x, a.y - b.y, a.z - b.z)
      }
    }.toMap

  def toCoordinates(xs: Array[Int]): Coordinates = Coordinates(xs(0), xs(1), xs(2))

  val input: List[Set[Coordinates]] = readFile("resources/day19").foldLeft(List.empty[Set[Coordinates]]) {
    (acc, s) =>
      if (s.isBlank) acc
      else if (s.startsWith("--")) Set.empty :: acc
      else (acc.head + toCoordinates(s.split(",").map(_.toInt))) :: acc.tail
  }

  @tailrec
  def merge(
             matchedScanners: List[Set[Coordinates]],
             unmatchedScanners: List[Set[Coordinates]],
             shifts: List[Coordinates] = List(Coordinates(0, 0, 0))
           ): (Set[Coordinates], List[Coordinates]) =
    if (unmatchedScanners.isEmpty) (matchedScanners.reduce(_ union _), shifts)
    else {
      val scannerDistances = relativeDistances(matchedScanners.head)

      val otherScanners = unmatchedScanners.map { otherScanner =>
        (for {
          turnedScanner <- allTurns(otherScanner)
          (otherPoint, otherDistances) <- relativeDistances(turnedScanner)
          (point, distances) <- scannerDistances
          if distances.intersect(otherDistances).size > 10
          shift = point.distance(otherPoint)
        } yield turnedScanner.map(_.shift(shift)) -> shift).headOption match {
          case Some((s, p)) => Right((s, p))
          case _ => Left(otherScanner)
        }
      }.distinct

      val matchingScanners = otherScanners.flatMap(_.toSeq)

      if (matchingScanners.isEmpty)
        merge(matchedScanners.tail.appended(matchedScanners.head), unmatchedScanners, shifts)
      else
        merge(
          matchingScanners.map(_._1).appendedAll(matchedScanners),
          otherScanners.flatMap(_.left.toSeq),
          matchingScanners.map(_._2).appendedAll(shifts)
        )
    }

  val (fullMap, shiftPoints) = merge(List(input.head), input.tail)

  println(fullMap.size)
  println(shiftPoints.flatMap { p => shiftPoints.map(p.manhattan) }.max)
}
