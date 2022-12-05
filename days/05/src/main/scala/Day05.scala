import better.files.Resource

trait CrateMover:
    def rearrange(initial: List[String], order: Rearrangement): List[String]

object CrateMover9000 extends CrateMover {
    def rearrangeSingle(initial: List[String], from: Int, to: Int): List[String] =
        val originStack = initial(from - 1)
        initial
            .updated(from - 1, originStack.tail)
            .updated(to - 1, originStack.head + initial(to - 1))
    def rearrange(initial: List[String], order: Rearrangement): List[String] = (1 to order.count).foldLeft(initial) { (input, _) => 
        (1 to order.count).foldLeft(initial) { (res, _) => rearrangeSingle(res, order.from, order.to) }
    }
}

object CrateMover9001 extends CrateMover {
    def rearrange(initial: List[String], order: Rearrangement): List[String] = (1 to order.count).foldLeft(initial) { (input, _) =>
        val originStack = initial(order.from - 1)
        initial
            .updated(order.from - 1, originStack.drop(order.count))
            .updated(order.to - 1, originStack.take(order.count) + initial(order.to - 1))
    }
}

case class Rearrangement(count: Int, from: Int, to: Int)
case class CraneYard(stacks: List[String]) {
    def rearrange(order: Rearrangement)(using mover: CrateMover): CraneYard = CraneYard(mover.rearrange(stacks, order))
    def rearrange(orders: Iterable[Rearrangement])(using CrateMover): CraneYard = orders.foldLeft(this) { (res, order) => res.rearrange(order) }
}

object CraneYard:
    def fromInput(input: List[String]): CraneYard =
        val crates = input.dropRight(1)
        val crateCount = crates.last.size / 4
        // Assuming a crate is always a single character
        def readStack(no: Int): String = crates.reverse.foldLeft("") { (stack, line) => line.charAt(1 + 4*no) + stack}.trim()
        CraneYard((0 to crateCount).map(readStack).toList)

object Rearrangement:
    val regex = """move (\d+) from (\d+) to (\d+)""".r
    def fromLine(line: String): Rearrangement = line match
        case regex(count, from, to) => Rearrangement(count.toInt, from.toInt, to.toInt)
        case illegal => throw IllegalArgumentException(s"Line '$line' is not a valid re-arrangement line!")

def readInput(input: String): (CraneYard, List[Rearrangement]) =
    val lines = input.split("\n")
    val yard = lines.takeWhile(_.nonEmpty).toList
    val craneYard = CraneYard.fromInput(yard)
    val rearrangements = lines.drop(yard.size + 1).map(Rearrangement.fromLine).toList
    (craneYard, rearrangements)



@main def part1(): Unit = {
    given mover: CrateMover = CrateMover9000
    val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
    val (craneYard, rearrangements) = readInput(input)
    val resultYard = craneYard.rearrange(rearrangements)
    println(s"The resulting head crates are '${resultYard.stacks.map(_.head).mkString}'")
}

@main def part2(): Unit = {
    given mover: CrateMover = CrateMover9001
    val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
    val (craneYard, rearrangements) = readInput(input)
    val resultYard = craneYard.rearrange(rearrangements)
    println(s"The resulting head crates are '${resultYard.stacks.map(_.head).mkString}'")
}