import better.files.Resource

case class Rucksack(content: String) {
    def compartments(compartmentCount: Int): List[String] = 
        val groupSize = content.size / compartmentCount
        require(content.size % compartmentCount == 0, s"Can't split this Rucksack of size ${content.size} into equal compartment of size ${compartmentCount} with $groupSize each!" +
          s"\n\t${content.size % compartmentCount} would be left!")
        content.grouped(groupSize).toList
    def commonInAllCompartments(compartmentCount: Int): Set[Char] = compartments(compartmentCount).map(_.toSet).reduce(_ intersect _)
}

extension (c: Char)
    def priority: Int = 
        require("[a-zA-Z]".r.matches(c.toString()), s"The character '$c' has no defined priority")
        if (c.isUpper) c.toInt - 38 // ASCII Codes for uppercase chars start at 65, but priorities start at 27
        else c.toInt - 96           // ASCII Codes for lowercase chars at 97, but their priority start at 1

def readRucksacks(input: String): List[Rucksack] = input.split("\n").map(Rucksack.apply).toList


@main def part1(): Unit = {
    val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
    val rucksacks = readRucksacks(input)
    val priorities = rucksacks
        .map(_.commonInAllCompartments(2))
        .map(_.head.priority)
    println(s"The sum of all priorities is ${priorities.sum}")
}

extension (rs: List[Rucksack])
    def commonItemBadge: Char =
        val common = rs.map(_.content.toSet).reduce(_ intersect _)
        require(common.size == 1, s"This group has more items in common: ${common.mkString("('", "', '", "')")}")
        common.head

@main def part2(): Unit = {
    val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
    val rucksacks = readRucksacks(input)
    val groupPriorities = rucksacks.grouped(3)
        .map(_.commonItemBadge.priority)
    println(s"The sum of all group priorities is ${groupPriorities.sum}")
}