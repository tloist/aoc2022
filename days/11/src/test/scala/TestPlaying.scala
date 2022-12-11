import better.files.Resource
import munit.FunSuite

class TestPlaying extends FunSuite {
  def exampleInput = Resource.getAsString("example.txt")
  def exampleMonkeys = parseMonkeys(exampleInput)
  def exampleInitField = MonkeyPlayfield(exampleMonkeys, relieveFactor = 3)

  test("Playing the monkey sheningans #10") {
    val field10 = exampleInitField.rounds(10)
    assertEquals(field10.monkeys(0).items, items(91, 16, 20, 98))
    assertEquals(field10.monkeys(1).items, items(481, 245, 22, 26, 1092, 30))
  }

  test("Playing the monkey sheningans #15") {
    val field15 = exampleInitField.rounds(15)
    assertEquals(field15.monkeys(0).items, items(83, 44, 8, 184, 9, 20, 26, 102))
    assertEquals(field15.monkeys(1).items, items(110, 36))
  }

  test("Playing the monkey sheningans #20") {
    val field20 = exampleInitField.rounds(20)
    assertEquals(field20.monkeys(0).items, items(10, 12, 14, 26, 34))
    assertEquals(field20.monkeys(1).items, items(245, 93, 53, 199, 115))
  }

  test("Find monkey business level after round 20") {
    assertEquals(exampleInitField.rounds(20).monkeyBusinessLevel, BigInt(10605))
  }

  def exampleInitFieldPartB = MonkeyPlayfield.initializeWithNormalizer(exampleMonkeys)

  def checkMonkeyBusinessB(round: Int, expected: List[Int])(using loc: munit.Location): Unit =
    test(s"Part B: Checking monkey business after round $round") {
      val field = exampleInitFieldPartB.rounds(round)
      if (field.monkeyBusinesses != expected) {
        println(field.inspectMonkeyBusiness(round))
        println()
        println(field.inspectItems(round))
      }
      assertEquals(field.monkeyBusinesses, expected)
    }

  checkMonkeyBusinessB(1, List(2, 4, 3, 6))
  checkMonkeyBusinessB(20, List(99, 97, 8, 103))
  checkMonkeyBusinessB(1000, List(5204, 4792, 199, 5192))
  checkMonkeyBusinessB(5000, List(26075, 23921, 974, 26000))
  checkMonkeyBusinessB(10000, List(52166, 47830, 1938, 52013))

  test("Part B: Monkey Business after round 10000") {
    assertEquals(exampleInitFieldPartB.rounds(10000).monkeyBusinessLevel, BigInt("2713310158"))
  }

  def items(items: Int*): List[BigInt] = items.map(BigInt.apply).toList
}
