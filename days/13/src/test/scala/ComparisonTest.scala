import munit.FunSuite

class ComparisonTest extends FunSuite {
  val ordering: Ordering[Packet] = summon[Ordering[Packet]]

  test("Comparing numbers") {
    val sorted = List(number(8), number(5)).sorted
    assertEquals(sorted, List(number(5), number(8)))
  }

  test("Comparing two empty lists are equal") {
    assertEquals(ordering.compare(list(), list()), 0)
  }

  test("Comparing two list: First diff decides") {
    assertEquals(ordering.compare(listOfNumbers(1,2,3,4,9,9), listOfNumbers(1,2,3,5,0,0)), -1)
  }

  test("Comparing two list: Equal content but smaller one is smaller") {
    assertEquals(ordering.compare(listOfNumbers(0, 0, 0), listOfNumbers(0, 0, 0, 0)), -1)
  }

  test("Comparing mixed packets") {
    assertEquals(ordering.compare(listOfNumbers(0, 0, 0), number(2)).sign, -1)
    assertEquals(ordering.compare(listOfNumbers(0, 0, 0), number(0)).sign, 1)
  }

  private def number(no: Int) = Packet.Number(no)
  private def listOfNumbers(numbers: Int*) = Packet.List(numbers.map(Packet.Number.apply))
  private def list(packets: Packet*) = Packet.List(packets)
}
