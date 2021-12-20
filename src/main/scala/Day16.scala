import Util.{bigIntFromBinary, intFromBinary, readFile}

import scala.annotation.tailrec

@main def day16(): Unit = {

  sealed trait Packet {
    def version: Int
    def typeID: Int
  }

  case class Literal(version: Int, typeID: Int, value: BigInt) extends Packet
  case class Operator(version: Int, typeID: Int, packets: List[Packet]) extends Packet

  val input = readFile("resources/day16").head.map { x =>
    Integer.parseInt(x.toString, 16).toBinaryString.reverse.padTo(4, '0').reverse
  }.mkString("")

  def parsePacket(xs: String): (Packet, String) = {
    val version = intFromBinary(xs.take(3))
    val typeID = intFromBinary(xs.slice(3, 6))

    typeID match {
      case 4 =>
        val (literalValue, tail) = parseLiteralValue(xs.drop(6))
        Literal(version, typeID, literalValue) -> tail
      case _ =>
        xs(6) match {
          case '0' =>
            val totalLength = intFromBinary(xs.slice(7, 22))
            val packets = parsePackets(xs.slice(22, 22 + totalLength))
            Operator(version, typeID, parsePackets(xs.slice(22, 22 + totalLength))) -> xs.drop(22 + totalLength)
          case _ =>
            val (packets, tail) = (0 until intFromBinary(xs.slice(7, 18))).foldLeft((List.empty[Packet], xs.drop(18))) {
              case ((packets, s), _) =>
                val (packet, tail) = parsePacket(s)
                (packet :: packets) -> tail
            }
            Operator(version, typeID, packets) -> tail
        }
    }
  }

  @tailrec
  def parsePackets(xs: String, packets: List[Packet] = List.empty): List[Packet] =
    if (xs.isEmpty) packets
    else {
      val (packet, tail) = parsePacket(xs)
      parsePackets(tail, packet :: packets)
    }

  @tailrec
  def parseLiteralValue(xs: String, prefix: String = ""): (BigInt, String) = xs.head match {
    case '1' => parseLiteralValue(xs.drop(5), prefix + xs.tail.take(4))
    case _ => bigIntFromBinary(prefix + xs.tail.take(4)) -> xs.drop(5)
  }

  def versionsSum(packet: Packet): Int = packet match {
    case Operator(version, _, packets) => version + packets.map(versionsSum).sum
    case Literal(version, _, _) => version
  }

  def eval(packet: Packet): BigInt = packet match {
    case Operator(_, typeID, packets) => typeID match {
      case 0 => packets.map(eval).sum
      case 1 => packets.map(eval).product
      case 2 => packets.map(eval).min
      case 3 => packets.map(eval).max
      case 5 => if (eval(packets.last) > eval(packets.head)) 1 else 0
      case 6 => if (eval(packets.last) < eval(packets.head)) 1 else 0
      case 7 => if (eval(packets.last) == eval(packets.head)) 1 else 0
    }
    case Literal(_, _, value) => value
  }

  val parsedPacket = parsePacket(input)._1

  println(versionsSum(parsedPacket))
  println(eval(parsedPacket))
}
