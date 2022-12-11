import munit.FunSuite
import better.files.Resource

class TestParsing extends FunSuite {
  def exampleInput = Resource.getAsString("example.txt")

  test("Can read example input") {
    val monkeys = parseMonkeys(exampleInput)
    monkeys.foreach(println)

    assertEquals(monkeys(0).no, 0)
    assertEquals(monkeys(0).items, List(79, 98).map(BigInt.apply))
    assertEquals(monkeys(0).divisor, 23)
    assertEquals(monkeys(0).target._2, 3)
    assertEquals(monkeys(0).target._1, 2)

    assertEquals(monkeys(1).no, 1)
    assertEquals(monkeys(1).items, List(54, 65, 75, 74).map(BigInt.apply))
    assertEquals(monkeys(1).divisor, 19)
    assertEquals(monkeys(1).target._2, 2)
    assertEquals(monkeys(1).target._1, 0)

    assertEquals(monkeys(2).no, 2)
    assertEquals(monkeys(2).items, List(79, 60, 97).map(BigInt.apply))
    assertEquals(monkeys(2).divisor, 13)
    assertEquals(monkeys(2).target._2, 1)
    assertEquals(monkeys(2).target._1, 3)

    assertEquals(monkeys(3).no, 3)
    assertEquals(monkeys(3).items, List(74).map(BigInt.apply))
    assertEquals(monkeys(3).divisor, 17)
    assertEquals(monkeys(3).target._2, 0)
    assertEquals(monkeys(3).target._1, 1)
  }

}
