import better.files.Resource

case class Rearrangement(count: Int, from: Int, to: Int)
case class CraneYard(stacks: List[String]) {
    def rearrangeSingle(from: Int, to: Int): CraneYard =
        val originStack = stacks(from - 1)
        CraneYard(stacks
            .updated(from - 1, originStack.tail)
            .updated(to - 1, originStack.head + stacks(to - 1)))
    
    def rearrange(order: Rearrangement): CraneYard = (1 to order.count).foldLeft(this) { (res, _) => res.rearrangeSingle(order.from, order.to) }
    def rearrange(orders: Iterable[Rearrangement]): CraneYard = orders.foldLeft(this) { (res, order) => res.rearrange(order) }
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
    val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
    val (craneYard, rearrangements) = readInput(input)
    val resultYard = craneYard.rearrange(rearrangements)
    println(s"The resulting head crates are '${resultYard.stacks.map(_.head).mkString}'")

}