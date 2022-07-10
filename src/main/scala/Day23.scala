import Util.readFile

import scala.annotation.tailrec
import BurrowExtensions.*

import scala.collection.mutable

@main def day23(): Unit = {
  val input = readFile("resources/day23")

  val s1 = State.from(input)
  println(findMinEnergyImmutable(List(WeightedState(s1, 0))))

  val s2 = State.from(input.take(3) ++ List("  #D#C#B#A#  ", "  #D#B#A#C#  ") ++ input.takeRight(2))
  println(findMinEnergyImmutable(List(WeightedState(s2, 0))))
}

@tailrec
def findMinEnergyImmutable(
                            toVisit: List[WeightedState],
                            visited: Set[WeightedState] = Set.empty,
                            energy: Map[State, Int] = Map.empty.withDefaultValue(Int.MaxValue)
                          ): Option[Int] = {
  toVisit match {
    case x :: xs =>
      val newVisited = visited + x

      val newStates = for {
        next <- x.state.next()
        if !visited.contains(next)
        newEnergy = x.energy + next.energy
        if newEnergy < energy(next.state)
      } yield WeightedState(next.state, newEnergy)

      val newToVisit = xs ++ newStates
      val newEnergy = energy ++ newStates.map { s => s.state -> s.energy }.toMap

      findMinEnergyImmutable(newToVisit, newVisited, newEnergy)

    case _ => energy.find { case (k, _) => k.isComplete }.map(_._2)
  }
}

def findMinEnergyMutable(s: State): Option[Int] = {
  val toVisit = mutable.PriorityQueue[WeightedState](WeightedState(s, 0))
  val visited = mutable.Set[WeightedState]()
  val energy = mutable.Map[State, Int]().withDefaultValue(Int.MaxValue)

  while (toVisit.nonEmpty) {
    val current = toVisit.dequeue()
    visited.add(current)
    current.state.next().foreach { next =>
      if (!visited.contains(next)) {
        val newEnergy = current.energy + next.energy
        if (newEnergy < energy(next.state)) {
          energy += next.state -> newEnergy
          toVisit.addOne(WeightedState(next.state, newEnergy))
        }
      }
    }
  }

  energy.find { case (k, _) => k.isComplete }.map(_._2)
}

val energyMap: Map[Char, Int] = ('A' to 'D').map { char =>
  char -> Math.pow(10, char - 'A').toInt
}.toMap

case class Room(char: Char, index: Int, content: List[Char]) {
  def hasOnlyValidAmphipods: Boolean = content.forall(_ == char)

  def hasAmphipodsWithWrongType: Boolean = !content.forall(Seq('.', char).contains)
}

case class WeightedState(state: State, energy: Int) extends Comparable[WeightedState] {
  override def compareTo(o: WeightedState): Int = energy.compareTo(o.energy)
}

case class State(config: List[List[Char]]) {
  private val hallway: List[Char] = config.head

  private val rooms: Map[Char, Room] = ('A' to 'D').map { char =>
    val index = (char - 'A' + 1) * 2
    char -> Room(char, index, config.tail.map(x => x(index)))
  }.toMap

  def isComplete: Boolean = rooms.values.forall(_.hasOnlyValidAmphipods)

  def next(): List[WeightedState] = {
    val xs = for {
      (movingChar, movingIndex) <- hallwayMovableAmphibious
      room = rooms(movingChar)
      if hallwayIsClear(movingIndex, room.index)
    } yield {
      val positionInRoom = room.content.lastIndexOf('.') + 1
      WeightedState(
        State(
          config.zipWithIndex.map { (row, rowIndex) =>
            val mutableRow = mutable.ArraySeq.from(row)
            rowIndex match {
              case 0 => mutableRow(movingIndex) = '.'
              case x if x == positionInRoom => mutableRow(room.index) = movingChar
              case _ =>
            }
            mutableRow.toList
          }
        ),
        (Math.abs(movingIndex - room.index) + positionInRoom) * energyMap(movingChar)
      )
    }

    val ys = for {
      room <- roomsWithWrongAmphipods
      (charToMove, indexToMove) = room.content.zipWithIndex.find(x => !x._1.isEmpty).get
      hallwaySpot <- availableHallwaySpots
      if hallwayIsClear(hallwaySpot, room.index)
    } yield {
      val positionInRoom = indexToMove + 1
      WeightedState(
        State(
          config.zipWithIndex.map { (row, rowIndex) =>
            val mutableRow = mutable.ArraySeq.from(row)
            rowIndex match {
              case x if x == positionInRoom => mutableRow(room.index) = '.'
              case 0 => mutableRow(hallwaySpot) = charToMove
              case _ =>
            }
            mutableRow.toList
          }
        ),
        (Math.abs(room.index - hallwaySpot) + positionInRoom) * energyMap(charToMove)
      )
    }

    xs ++ ys
  }

  private def availableHallwaySpots: Seq[Int] =
    (0 to 10).filterNot(Seq(2, 4, 6, 8).contains).filter(hallway(_).isEmpty)

  private def hallwayMovableAmphibious: List[(Char, Int)] =
    hallway.zipWithIndex.filter { case (char, _) =>
      char.isLetter && !rooms(char).hasAmphipodsWithWrongType
    }

  private def roomsWithWrongAmphipods =
    rooms.values.filter(_.hasAmphipodsWithWrongType).toList

  private def hallwayIsClear(i: Int, j: Int): Boolean =
    hallway.slice(Math.min(i, j) + 1, Math.max(i, j)).forall(_.isEmpty)
}

object State {
  def from(input: List[String]): State = State(input.drop(1).dropRight(1).map {
    _.drop(1).dropRight(1).toList
  })
}

object BurrowExtensions {
  extension (place: Char) {
    def isEmpty: Boolean = place == '.'
  }
}
