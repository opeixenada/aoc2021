import Util.readFile

import scala.annotation.tailrec

case class Point(x: Int, y: Int)

case class Line(a: Point, b: Point) {
  def isOrthogonal: Boolean = a.x == b.x || a.y == b.y
}

@main def day05(): Unit = {
  val input: List[Line] = readFile("resources/day05").map { s =>
    val coordinates = s.split(" -> ").map(_.split(",").map(_.toInt))
    Line(Point(coordinates.head.head, coordinates.head.last), Point(coordinates.last.head, coordinates.last.last))
  }

  val onlyOrthogonalOverlaps = input.foldLeft(Map[Point, Int]()) { (map, line) =>
    if (line.isOrthogonal) {
      (for (x <- Math.min(line.a.x, line.b.x) to Math.max(line.a.x, line.b.x);
            y <- Math.min(line.a.y, line.b.y) to Math.max(line.a.y, line.b.y))
      yield Point(x, y))
        .foldLeft(map) { (map, point) => map + (point -> (map.getOrElse(point, 0) + 1)) }
    } else map
  }

  val allOverlaps = input.foldLeft(Map[Point, Int]()) { (map, line) =>
    val directionX = line.b.x.compare(line.a.x)
    val directionY = line.b.y.compare(line.a.y)
    val steps = if (directionX != 0) (line.a.x - line.b.x).abs else (line.a.y - line.b.y).abs
    (0 to steps).foldLeft(map) { (map, step) =>
      val point = Point(line.a.x + step * directionX, line.a.y + step * directionY)
      map + (point -> (map.getOrElse(point, 0) + 1))
    }
  }

  println(onlyOrthogonalOverlaps.values.count(_ > 1))
  println(allOverlaps.values.count(_ > 1))
}