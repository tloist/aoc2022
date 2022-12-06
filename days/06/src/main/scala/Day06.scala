import better.files.Resource

def indexOfMarkerEnd(signal: String, markerSize: Int): Option[Int] =
    signal.sliding(markerSize).find(_.toSet.size == markerSize).map(signal.indexOf).map(_ + markerSize)

val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))

@main def part1(): Unit = println(indexOfMarkerEnd(input, 4).map(res => s"The packet starts at '$res'").getOrElse("The packet doesn't start at all!"))
@main def part2(): Unit = println(indexOfMarkerEnd(input, 14).map(res => s"The message starts at '$res'").getOrElse("The message doesn't start at all!"))