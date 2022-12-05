import better.files.Resource
import munit.FunSuite

val exampleInput = Resource.asString("example.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
val expectedStacks = List("NZ","DCM","P")
val expectedOrders = List(
    Rearrangement(1, 2, 1),
    Rearrangement(3, 1, 3),
    Rearrangement(2, 2, 1),
    Rearrangement(1, 1, 2),
)
val exampleYard = CraneYard(expectedStacks)


class TestDay05 extends FunSuite {

    test("CraneYard is parsed correctly") {
        val (yard, _) = readInput(exampleInput)
        assertEquals(yard.stacks, expectedStacks)
    }

    test("Rearrangements are parsed correctly") {
        val (_, orders) = readInput(exampleInput)
        assertEquals(orders, expectedOrders)
    }

    test("Craneyard after 1 order is correct") {
        val result = exampleYard.rearrange(expectedOrders.head)
        assertEquals(result.stacks, List("DNZ", "CM", "P"))
    }

    test("Craneyard after 2 orders is correct") {
        val result = exampleYard.rearrange(expectedOrders.take(2))
        assertEquals(result.stacks, List("", "CM", "ZNDP"))
    }

    test("Craneyard after 3 orders is correct") {
        val result = exampleYard.rearrange(expectedOrders.take(3))
        assertEquals(result.stacks, List("MC", "", "ZNDP"))
    }

    test("Craneyard after all 4 orders is correct") {
        val result = exampleYard.rearrange(expectedOrders)
        assertEquals(result.stacks, List("C", "M", "ZNDP"))
    }
}
