import Util.readFile

import scala.annotation.tailrec

type Board = Seq[Seq[(Int, Boolean)]]

def isWinning(board: Board): Boolean = board.exists(_.forall(_._2)) ||
  (0 to 4).exists(i => board.forall(b => b(i)._2))

def score(board: Board): Int =
  board.foldLeft(0) { (a, x) => a + x.foldLeft(0) { (b, y) => b + (if (y._2) 0 else y._1) } }

def mark(board: Board, x: Int): Board = board.map(_.map { y => if (y._1 == x) y._1 -> true else y })

@tailrec
def findFirstWinnerScore(boards: Seq[Board], numbers: Seq[Int], lastNumber: Int = 0): Int =
  boards.find(isWinning).map(score).match {
    case Some(score) => score * lastNumber
    case _ => findFirstWinnerScore(boards.map(mark(_, numbers.head)), numbers.tail, numbers.head)
  }

@tailrec
def findLastWinnerScore(boards: Seq[Board], numbers: Seq[Int]): Int = {
  val (winning, notWinning) = boards.map(mark(_, numbers.head)).partition(isWinning)
  if (notWinning.isEmpty && winning.size == 1) score(winning.head) * numbers.head
  else findLastWinnerScore(notWinning, numbers.tail)
}

@main def day04(): Unit = {
  val input = readFile("resources/day04")

  val numbers = input.head.split(',').map(_.toInt)

  val boards: Seq[Board] =
    input.drop(2).sliding(5, 6).toSeq.map(_.map(_.sliding(2, 3).toSeq.map(_.trim.toInt -> false)))

  println(findFirstWinnerScore(boards, numbers))
  println(findLastWinnerScore(boards, numbers))
}