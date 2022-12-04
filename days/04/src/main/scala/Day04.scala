import better.files.Resource

case class Range(from: Int, to: Int) {
    def fullyContains(that: Range): Boolean = this.from <= that.from && this.to >= that.to
    def isSeparate(that: Range): Boolean = this.to < that.from || this.from > that.to
    def overlaps(that: Range): Boolean = !isSeparate(that)
}

object Range:
    def fromString(input: String): Range =
        val parts = input.split("-").map(_.toInt)
        require(parts.size == 2)
        Range(parts.min, parts.max)

def readLine(line: String): (Range, Range) =
    val ranges = line.split(",").map(Range.fromString)
    require(ranges.size == 2)
    (ranges(0), ranges(1))

def readInput(input: String): List[(Range, Range)] = input.split("\n").map(readLine).toList

extension (pair: (Range, Range))
    def oneFullyContainsAnother = (pair._1 fullyContains pair._2) || (pair._2 fullyContains pair._1)

@main def part1(): Unit = {
  val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
  val pairs = readInput(input)
  val fullyContainedCount = pairs.count(_.oneFullyContainsAnother)
  println(s"The input contains $fullyContainedCount pairs where the first contains the second one fully")
}

@main def part2(): Unit = {
  val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
  val pairs = readInput(input)
  val overlapsCount = pairs.count(_ overlaps _)
  println(s"The input contains $overlapsCount pairs where there is an overlap")
}