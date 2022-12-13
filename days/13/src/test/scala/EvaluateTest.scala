import better.files.Resource
import munit.FunSuite

class EvaluateTest extends FunSuite {
  import math.Ordered.orderingToOrdered
  private def readInput = PacketParser.input(Resource.getAsString("example.txt"))

  test("Example evaluates to 13") {
    val pairs = readInput.getOrElse(fail("Failed to read input"))
    val correctOrder = pairs.zipWithIndex.collect{ case ((left, right), i) if left < right => i+1 }
    println(correctOrder)
    assertEquals(correctOrder.size, 4)
    assertEquals(correctOrder.sum, 13)
  }

  test("Part B") {
    val sorted = asSortedSeparatedPackets(readInput.getOrElse(fail("Failed to read input")))
    sorted.foreach(println)
    assertEquals(sorted.head, Packet.List(Seq.empty), s"First element in sorted list isn't the empty list")
    val index2 = sorted.indexOf(separator(2))
    val index6 = sorted.indexOf(separator(6))
    assertEquals(index2, 9)
    assertEquals(index6, 13)

  }

}
