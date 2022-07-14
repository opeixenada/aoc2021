import Util.readFile

import scala.annotation.tailrec

@main def day24(): Unit = {
  val inputParameters = readFile("resources/day24").grouped(18).map(extractParams).toList

  println(findInput(List(AluState(inputParameters.reverse, 0, Nil)), (1 to 9).reverse).map(_.mkString))
  println(findInput(List(AluState(inputParameters.reverse, 0, Nil)), 1 to 9).map(_.mkString))
}

case class AluState(
                     ps: List[(Int, Int, Int)],
                     z: Int,
                     foundInput: List[Int],
                   ) {
  def next(inputs: Range): Either[List[AluState], List[Int]] = ps match {
    case (k1, k2, k3) :: ks =>
      val xs = for {
        i <- inputs.toList
        z0 <- getZ0(z, k1, k2, k3, i)
      } yield {
        AluState(ks, z0, i :: foundInput)
      }
      Left(xs)
    case Nil if z == 0 => Right(foundInput)
    case _ => Left(Nil)
  }
}

@tailrec
def findInput(states: List[AluState], inputs: Range): Option[List[Int]] = states match {
  case x :: xs => x.next(inputs) match {
    case Right(v) => Some(v)
    case Left(ys) => findInput(ys.appendedAll(xs), inputs)
  }
  case _ => None
}

def extractParams(xs: List[String]): (Int, Int, Int) = {
  val p1 = "div z (-?\\d+)".r
  val p1(x1) = xs(4)

  val p2 = "add x (-?\\d+)".r
  val p2(x2) = xs(5)

  val p3 = "add y (-?\\d+)".r
  val p3(x3) = xs(15)

  (x1.toInt, x2.toInt, x3.toInt)
}

def getZ(z0: Int, k1: Int, k2: Int, k3: Int, x: Int): Int =
  if (z0 % 26 == x - k2) z0 / k1
  else z0 / k1 * 26 + x + k3

def getZ0(z: Int, k1: Int, k2: Int, k3: Int, x: Int): List[Int] = {
  List(
    if ((z - x - k3) % 26 == 0) Some((z - x - k3) / 26 * k1)
    else None,

    if ((z * k1 + x - k2) % 26 == x - k2) Some(z * k1 + x - k2)
    else None
  ).flatten
}
