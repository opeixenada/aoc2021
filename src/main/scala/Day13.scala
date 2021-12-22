import Util.{Coordinates, readFile}

import scala.annotation.tailrec

@main def day13(): Unit = {
  val input = readFile("resources/day13")

  val coordinates: List[Coordinates] = input.takeWhile(!_.isBlank).map { x =>
    val split = x.split(",").map(_.toInt)
    split.head -> split.last
  }

  val folds = input.drop(coordinates.length + 1).map { x =>
    val split = x.stripPrefix("fold along ").split("=")
    val axis = split.head
    val coordinate = split.last.toInt
    axis -> coordinate
  }

  def fold(input: List[Coordinates], fold: (String, Int)): List[Coordinates] = input.map { case (x, y) =>
    fold._1 match
      case "x" if x > fold._2 => (2 * fold._2 - x) -> y
      case "y" if y > fold._2 => x -> (2 * fold._2 - y)
      case _ => x -> y
  }.distinct

  println(fold(coordinates, folds.head).size)

  val xs = folds.foldLeft(coordinates)(fold)

  println(
    (0 to xs.map(_._2).max).map { y =>
      (0 to xs.map(_._1).max).map { x =>
        if (xs.contains(x -> y)) '#' else '.'
      }.mkString("")
    }.mkString("\n"))
}
