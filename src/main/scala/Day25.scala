import Util.readFile

import scala.annotation.tailrec

type Cucumbers = IndexedSeq[IndexedSeq[Char]]

@main def day25(): Unit = {
  val input = readFile("resources/day25")

  val cs: Cucumbers = input.toIndexedSeq.map(_.toIndexedSeq)

  println(countMoves(cs))
}

def print(cs: Cucumbers): Unit = println(cs.map(_.mkString).mkString("\n"))

def transposeClockwise(cs: Cucumbers): Cucumbers =
  cs.head.indices.map { y =>
    cs.indices.map { x =>
      cs(x)(cs.head.size - 1 - y) match {
        case 'v' => '>'
        case '<' => 'v'
        case _ => '.'
      }
    }
  }

def transposeCounterClockwise(cs: Cucumbers): Cucumbers =
  cs.head.indices.map { y =>
    cs.indices.map { x =>
      cs(cs.indices.size - 1 - x)(y) match {
        case '>' => 'v'
        case 'v' => '<'
        case x => x
      }
    }
  }

@tailrec
def countMoves(cs: Cucumbers, count: Int = 1): Int = {
  val (cs2, moved) = move(cs)
  if (moved) countMoves(cs2, count + 1)
  else count
}

def move(cs: Cucumbers): (Cucumbers, Boolean) = {
  val move1 = transposeClockwise(moveDown(transposeCounterClockwise(cs)))
  val move2 = moveDown(move1)
  (move2, move2 != cs)
}

def moveDown(cs: Cucumbers): Cucumbers = {
  val rs = cs.zip(cs.tail.appended(cs.head)).zip(cs.drop(2) ++ cs.take(2)).map { case ((xs, ys), zs) =>
    xs.zip(ys).zip(zs).map {
      case ((_, 'v'), '.') => '.'
      case (('v', '.'), _) => 'v'
      case ((_, y), _) => y
    }
  }

  rs.takeRight(1) ++ rs.dropRight(1)
}
