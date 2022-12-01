import better.files.Resource

def readElvesCalories(): List[List[Int]] = {
  val raw = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
  val (elf, elves) = raw.split("\n").foldLeft((List.empty[Int], List.empty[List[Int]])) { (current, line) =>
    val (elf, elves) = current
    if (line.isBlank()) (List.empty, elves :+ elf)
    else (elf :+ line.toInt, elves)
  }
  if (elf.nonEmpty) elves :+ elf else elves
}


@main def part1(): Unit = {
  val elves = readElvesCalories()
  val hungriest = elves.maxBy(_.sum)
  println("The hungriest elf carries:")
  println(hungriest.map(n => s"\t$n"))
  println(s"For a total of ${hungriest.sum}")
}

@main def part2(): Unit = {
  val elves = readElvesCalories()
  val byHunger = elves.map(_.sum).sorted.reverse
  val top3inSum = byHunger.take(3)
  println(s"The hungriest thre elves carry $top3inSum which is a total of ${top3inSum.sum} calories in total")
}