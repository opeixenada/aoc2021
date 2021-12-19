import java.lang.Math.{max, min}
import scala.io.Source

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }
  
  def intList(filename: String): List[Int] = readFile(filename).head.split(',').map(_.toInt).toList
  
  def intMatrix(filename: String): IndexedSeq[IndexedSeq[Int]] = readFile(filename).map(_.map(_.toString.toInt)).toIndexedSeq

  type Coordinates = (Int, Int)

  def getNeighbours(i: Int, j: Int, input: IndexedSeq[IndexedSeq[Int]]): Seq[Coordinates] =
    (for (x <- max(0, i - 1) to min(i + 1, input.length - 1);
          y <- max(0, j - 1) to min(j + 1, input.head.length - 1))
    yield (x, y))
      .filterNot((x, y) => x == i && y == j)

  def intFromBinary(x: String): Int = Integer.parseInt(x, 2)

  def bigIntFromBinary(x: String): BigInt = BigInt(x, 2)
}
