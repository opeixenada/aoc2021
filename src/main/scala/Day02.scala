import Util.readFile

import scala.io.Source

def applyCommand(coordinates: (Int, Int), s: String): (Int, Int) = {
  val (x, y) = coordinates
  val (command, v) = parseCommand(s)
  command match {
    case "forward" => (x + v, y)
    case "down" => (x, y + v)
    case "up" => (x, y - v)
  }
}

def applyCommandWithAim(position: (Int, Int, Int), s: String): (Int, Int, Int) = {
  val (x, y, aim) = position
  val (command, v) = parseCommand(s)
  command match {
    case "forward" => (x + v, y + (aim * v), aim)
    case "down" => (x, y, aim + v)
    case "up" => (x, y, aim - v)
  }
}

def parseCommand(s: String): (String, Int) = {
  val split = s.split(' ')
  (split(0), split(1).toInt)
}

@main def day02(): Unit = {
  val input = readFile("resources/day02")

  val result = input.foldLeft((0, 0))(applyCommand)
  println(result._1 * result._2)

  val resultWithAim = input.foldLeft((0, 0, 0))(applyCommandWithAim)
  println(resultWithAim._1 * resultWithAim._2)
}