import cats.data.NonEmptyList
import better.files.Resource
import math.Ordered.orderingToOrdered

enum Packet:
  case Number(value: Int)
  case List(values: Seq[Packet])
  override def toString: String = this match
    case Number(v) => v.toString
    case List(vs) => vs.mkString("[", ",", "]")

object Packet:
  given Ordering[Packet] with
    override def compare(left: Packet, right: Packet): Int = (left, right) match
      case (Number(noLeft), Number(noRight)) => noLeft - noRight
      case (List(valsLeft), List(valsRight)) => compareLists(valsLeft, valsRight)
      case (pNo: Number, List(vals)) => compareLists(Seq(pNo), vals)
      case (List(vals), pNo: Number) => compareLists(vals, Seq(pNo))

  private def compareLists(lefts: Seq[Packet], rights: Seq[Packet])(using order: Ordering[Packet]): Int =
    val elemCompare = (lefts zip rights).foldLeft(0) { (res, values) =>
      val (left, right) = values
      if res == 0 then order.compare(left, right) else res
    }
    if elemCompare == 0 then lefts.size - rights.size else elemCompare

object PacketParser {
  import cats.parse.Parser
  import cats.parse.Parser.void
  import cats.parse.Rfc5234.{digit, crlf, cr, lf}

  private val listStart: Parser[Unit] = cats.parse.Parser.char('[').void
  private val listSep: Parser[Unit] = cats.parse.Parser.char(',').void
  private val listEnd: Parser[Unit] = cats.parse.Parser.char(']').void
  private val newline: Parser[Unit] = Parser.oneOf(crlf :: cr :: lf :: Nil)

  val packet: Parser[Packet] = Parser.recursive[Packet] { recurse =>
    val no: Parser[Packet] = digit.rep.map(_.foldLeft("")(_ + _).toInt).map(Packet.Number.apply)
    val list: Parser[Packet] = (listStart *> recurse.repSep0(listSep) <* listEnd).map(Packet.List.apply)
    Parser.oneOf(no :: list :: Nil)
  }

  val packets: Parser[(Packet, Packet)] = (packet <* newline) ~ packet
  val packetPairs: Parser[NonEmptyList[(Packet, Packet)]] = packets.repSep(newline *> newline)

  def packet(input: String): Either[Parser.Error, Packet] = packet.parseAll(input)
  def pair(input: String): Either[Parser.Error, (Packet, Packet)] = packets.parseAll(input)
  def input(input: String): Either[Parser.Error, NonEmptyList[(Packet, Packet)]] = packetPairs.parseAll(input)
}

@main def part1 =
  PacketParser.input(Resource.getAsString("inputA.txt")) match
    case Left(error) => System.err.println(s"Failed to parse input due to $error")
    case Right(packetPairs) =>
      val indices = packetPairs.zipWithIndex.collect{ case ((left, right), i) if left < right => i+1 }
      println(s"The indices of the correct ordered packet pairs are ${indices.mkString(", ")} with a sum of ${indices.sum}")

def separator(no: Int): Packet = Packet.List(Seq(Packet.List(Seq(Packet.Number(no)))))
def asSortedSeparatedPackets(pairs: NonEmptyList[(Packet, Packet)]): List[Packet] =
  (pairs.toList.flatMap(p => Seq(p._1, p._2)) ++ Seq(separator(2), separator(6))).sorted

@main def part2 =
  PacketParser.input(Resource.getAsString("inputA.txt")) match
    case Left(error) => System.err.println(s"Failed to parse input due to $error")
    case Right(packetPairs) =>
      val separated = asSortedSeparatedPackets(packetPairs)
      val index2 = separated.indexOf(separator(2)) + 1
      val index6 = separated.indexOf(separator(6)) + 1
      println(s"Separator 2/6 are on index $index2/$index6 which results in ${index2 * index6}")