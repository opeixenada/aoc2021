import Util.readFile

import scala.io.Source

def applyCommand(coordinates: (Int, Int), command: String): (Int, Int) = {
  val (x, y) = coordinates
  val split = command.split(' ')
  val v = split(1).toInt
  split.head match {
    case "forward" => (x + v, y)
    case "down" => (x, y + v)
    case "up" => (x, y - v)
  }
}

def applyCommandWithAim(position: (Int, Int, Int), command: String): (Int, Int, Int) = {
  val (x, y, aim) = position
  val split = command.split(' ')
  val v = split(1).toInt
  split.head match {
    case "forward" => (x + v, y + (aim * v), aim)
    case "down" => (x, y, aim + v)
    case "up" => (x, y, aim - v)
  }
}

@main def day02(): Unit = {
  val input = readFile("resources/day02")

  val result = input.foldLeft((0, 0))(applyCommand)
  println(result._1 * result._2)

  val resultWithAim = input.foldLeft((0, 0, 0))(applyCommandWithAim)
  println(resultWithAim._1 * resultWithAim._2)
}
