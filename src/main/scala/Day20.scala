import Util.readFile

import java.util.UUID
import scala.annotation.tailrec

@main def day20(): Unit = {
  val input = readFile("resources/day20")

  def toBinaryChars(xs: Iterable[Char]) = xs.map {
    case '.' => '0'
    case _ => '1'
  }

  val algorithm = toBinaryChars(input.head).toIndexedSeq

  val image: IndexedSeq[IndexedSeq[Char]] = input.drop(2).map(toBinaryChars(_).toIndexedSeq).toIndexedSeq

  def enhance(image: IndexedSeq[IndexedSeq[Char]], default: Char): IndexedSeq[IndexedSeq[Char]] = {
    (0 to image.size + 1).map { x =>
      (0 to image.head.size + 1).map { y =>
        val ns = for {
          a <- x - 2 to x
          b <- y - 2 to y
        } yield try {
          image(a)(b)
        } catch { _ => default }

        algorithm(Integer.parseInt(ns.mkString(""), 2))
      }
    }
  }

  def getResult(iterations: Int) = (0 until iterations).foldLeft((image, '0')) { case ((image, default), _) =>
    (
      enhance(image, default),
      default match
        case '0' => algorithm(0)
        case '1' => algorithm(Integer.parseInt("111111111", 2))
    )
  }._1.foldLeft(0) { (acc, row) =>
    acc + row.count(_ == '1')
  }

  println(getResult(2))
  println(getResult(50))
}
