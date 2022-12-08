import better.files.Resource

@main def part1 =
  val map = TreeMap.fromResource("inputA.txt")
  println(s"There are ${map.visibleTreeLocations.size} trees visible on that Map")

@main def part2 =
  val map = TreeMap.fromResource("inputA.txt")
  val highestScenicScore = map.content.keys.map(p => p -> map.scenicScore(p)).maxBy(_._2)
  println(s"The highest scenic score is ${highestScenicScore._2} at the trees at ${highestScenicScore._1}")