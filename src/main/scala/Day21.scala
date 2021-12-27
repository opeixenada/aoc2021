import Util.readFile

import java.util.UUID
import scala.annotation.tailrec

@main def day21(): Unit = {
  val input = readFile("resources/day21").map(_.drop("Player 1 starting position: ".length).toInt)

  def getNewPosition(position: Int, score: Int) = (position + score) % 10 match
    case 0 => 10
    case x => x

  @tailrec
  def playDeterministic(positions: List[(Int, Int)], rolls: Int = 0): Int = {
    val newPosition = getNewPosition(positions.head._1, (1 to 3).map(x => (rolls + x) % 100).sum)
    val newScore = positions.head._2 + newPosition
    if (newScore >= 1000) positions.last._2 * (rolls + 3)
    else playDeterministic(positions.tail.appended(newPosition -> newScore), rolls + 3)
  }

  println(playDeterministic(input.map(_ -> 0)))

  case class PlayerState(position: Int, score: Int, otherPosition: Int, otherScore: Int, turn1: Boolean) {
    def next: List[PlayerState] =
      (for {
        x <- 1 to 3
        y <- 1 to 3
        z <- 1 to 3
      } yield x + y + z).map { x =>
        val newPosition = getNewPosition(position, x)
        PlayerState(otherPosition, otherScore, newPosition, score + newPosition, !turn1)
      }.toList
  }

  @tailrec
  def playDirac(
                 state: List[(PlayerState, BigInt)],
                 winCount1: BigInt = BigInt(0),
                 winCount2: BigInt = BigInt(0)
               ): BigInt =
    if (state.isEmpty) if (winCount1 > winCount2) winCount1 else winCount2
    else {
      val (winningStates, notWinningStates) = state
        .flatMap { (playerState, counts) => playerState.next.map(_ -> counts) }
        .groupBy(_._1).map { (k, v) => k -> v.map(_._2).reduce(_ + _) }.toList
        .partition(_._1.otherScore > 20)

      playDirac(
        notWinningStates,
        winCount1 + winningStates.filter(_._1.turn1).map(_._2).sum,
        winCount2 + winningStates.filterNot(_._1.turn1).map(_._2).sum
      )
    }

  println(playDirac(List(PlayerState(input.head, 0, input.last, 0, true) -> 1)))
}
