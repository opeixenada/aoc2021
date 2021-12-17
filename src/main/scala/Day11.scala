import Util.{Coordinates, intMatrix, getNeighbours}

import scala.annotation.tailrec

@main def day11(): Unit = {
  val input: IndexedSeq[IndexedSeq[Int]] = intMatrix("resources/day11")

  @tailrec
  def flash(
             input: IndexedSeq[IndexedSeq[Int]],
             flashed: Seq[Coordinates] = Seq.empty
           ): (IndexedSeq[IndexedSeq[Int]], Int) = {
    val newFlashed = input.zipWithIndex.flatMap { case (row, x) =>
      row.zipWithIndex.flatMap { case (energy, y) =>
        if (energy > 9) Some((x, y))
        else None
      }
    }

    if (newFlashed.isEmpty) (input, flashed.size)
    else {
      val allFlashed = flashed.appendedAll(newFlashed)

      val afterFlash = newFlashed
        .flatMap { case (x, y) => getNeighbours(x, y, input) }
        .foldLeft(input) { case (energies, (x, y)) =>
          energies.take(x)
            .appended(
              energies(x).take(y)
                .appended(energies(x)(y) + 1)
                .appendedAll(energies(x).drop(y + 1))
            )
            .appendedAll(energies.drop(x + 1))
        }
        .zipWithIndex.map { case (row, x) =>
          row.zipWithIndex.map { case (energy, y) => if (allFlashed.contains((x, y))) 0 else energy }
        }

      flash(afterFlash, allFlashed)
    }
  }

  def step(input: IndexedSeq[IndexedSeq[Int]]): (IndexedSeq[IndexedSeq[Int]], Int) = flash(input.map(_.map(_ + 1)))

  val flashesCount = (1 to 100).foldLeft((input, 0)) { case ((xss, count), i) =>
    val (yss, flashes) = step(xss)
    (yss, count + flashes)
  }

  println(flashesCount._2)

  @tailrec
  def simultaneousFlash(input: IndexedSeq[IndexedSeq[Int]], iteration: Int = 0): Int = {
    val (xss, flashes) = step(input)
    if (flashes == 100) iteration + 1
    else simultaneousFlash(xss, iteration + 1)
  }

  println(simultaneousFlash(input))
}