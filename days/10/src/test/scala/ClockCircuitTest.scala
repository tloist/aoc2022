import ClockCircuit._
import munit.FunSuite
import better.files.Resource

class ClockCircuitTest extends FunSuite {
  private def smallExample: String = Resource.getAsString("example1.txt")
  private def circuitSmallExample: ClockCircuit = ClockCircuit.init(ClockCircuit.parseInput(smallExample))
  private val addX3 = AddRegister(Register.X, BigInt(3))
  private val minuxX5 = AddRegister(Register.X, BigInt(-5))

  test("Small example: Parsing input") {
    val effects = ClockCircuit.parseInput(smallExample)
    assertEquals(effects.size, 3)
    assertEquals(effects(0), NoOperation)
    assertEquals(effects(1), AddRegister(Register.X, BigInt(3)))
    assertEquals(effects(2), AddRegister(Register.X, BigInt(-5)))
  }

  test("Small example: Executing") {
    val states = circuitSmallExample.ticks

    val circuit1 = states(0) // Noop
    assertEquals(circuit1.x, BigInt(1))
    assertEquals(circuit1.looming.head, LoomingEffect(NoOperation, 1))

    val circuit2 = states(1)  // addx 3 1/2
    assertEquals(circuit2.x, BigInt(1))
    assertEquals(circuit2.looming.head, LoomingEffect(addX3, 2))

    val circuit3 = states(2)   // addx 3 2/2
    assertEquals(circuit3.x, BigInt(1))
    assertEquals(circuit3.looming.head, LoomingEffect(addX3, 1))

    val circuit4 = states(3) // addx -5 1/2
    assertEquals(circuit4.x, BigInt(4))
    assertEquals(circuit4.looming.head, LoomingEffect(minuxX5, 2))

    val circuit5 = states(4)
    assertEquals(circuit5.x, BigInt(4))
    assertEquals(circuit5.looming.head, LoomingEffect(minuxX5, 1))

    val circuit6 = states(5)
    assertEquals(circuit6.x, BigInt(-1))
    assertEquals(circuit6.looming, List.empty)
  }

  test("Small example: Executing all at once") {
    val result = circuitSmallExample.ticks.last
    assertEquals(result.clock, 6)
    assertEquals(result.x, BigInt(-1))
    assertEquals(result.looming, List.empty)
  }

  private def circuitBigExample: ClockCircuit = ClockCircuit.init(ClockCircuit.parseInput(Resource.getAsString("example2.txt")))

  test("Big example") {
    val result = circuitBigExample.ticks.signalStrength.sum
    assertEquals(result, BigInt(13140))
  }

  test("Draw the expected image") {
    val expected =
      """|##..##..##..##..##..##..##..##..##..##..
         |###...###...###...###...###...###...###.
         |####....####....####....####....####....
         |#####.....#####.....#####.....#####.....
         |######......######......######......####
         |#######.......#######.......#######.....""".stripMargin
    val image = circuitBigExample.ticks.foldLeft("") { (res, circuit) =>
      res + (if ((circuit.clock - 1) % 40 - circuit.x).abs <= 1 then "#" else ".")
    }.grouped(40).mkString("\n")
    assertEquals(image, expected)
  }
}
