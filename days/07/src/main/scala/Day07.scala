import better.files.Resource

val input = Resource.asString("inputA.txt").getOrElse(sys.error("Couldn't read input file"))

@main def part1 =
    val shell = Shell.newSystem.adapt(InputParser.parse(input))
    val relevantDirs = shell.root.filesRecursively.collect {
        case dir: Directory if dir.size <= 100000 => dir.size 
    }
    println(s"There are ${relevantDirs.size} directories with a total size of '${relevantDirs.sum}'")

val requiredForUpdate = BigInt("30000000")

@main def part2 =
    val shell = Shell.newSystem.adapt(InputParser.parse(input))
    val taken = shell.root.size
    val required = requiredForUpdate - shell.totalFreeSize
    val found = shell.root.filesRecursively.collect {
        case dir: Directory if dir.size >= required => dir
    }.minByOption(_.size)
    println(found.map(dir => 
        s"We should delete '${dir.fullPath}' because it will restore '${dir.size}' bytes."
        ).getOrElse("There is no directory matching the criteria"))