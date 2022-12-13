import better.files.Resource
import munit.FunSuite

class ParserTest extends FunSuite {

  test("Simple number can be parsed") {
    val res = PacketParser.packet("9")
    assertEquals(res, Right(number(9)))
  }

  test("Empty list can be parsed") {
    val res = PacketParser.packet("[]")
    assertEquals(res, Right(listOfNumbers()))
  }

  test("List with simple numbers can be parsed") {
    val res = PacketParser.packet("[1,1,3,1,1]")
    assertEquals(res, Right(listOfNumbers(1,1,3,1,1)))
  }

  test("Nested lists can be parsed") {
    val res = PacketParser.packet("[[1],[2,3,4]]")
    assertEquals(res, Right(list(listOfNumbers(1), listOfNumbers(2,3,4))))
  }

  test("A pair can be parsed") {
    val pair =
      """|[1,1,3,1,1]
         |[1,1,5,1,1]""".stripMargin
    val res = PacketParser.pair(pair).getOrElse(fail(s"Couldn't parse pair"))
    assertEquals(res._1, listOfNumbers(1,1,3,1,1))
    assertEquals(res._2, listOfNumbers(1,1,5,1,1))
  }

  test("whole input can be parsed") {
    val res = PacketParser.input(Resource.getAsString("example.txt"))
      .getOrElse(fail(s"Couldn't parse whole input"))
    assertEquals(res.size, 8)

  }

  private def number(no: Int) = Packet.Number(no)
  private def listOfNumbers(numbers: Int*) = Packet.List(numbers.map(Packet.Number.apply))
  private def list(packets: Packet*) = Packet.List(packets)

}
