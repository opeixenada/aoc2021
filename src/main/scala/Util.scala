import scala.io.Source

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }

  def getBit(x: Int, position: Int): Int = (x >> position) & 1

  def setBit(x: Int, position: Int): Int = x | (1 << position)
}
