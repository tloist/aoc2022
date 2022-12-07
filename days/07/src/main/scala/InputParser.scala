case class CommandWithOutput[O](command: Command[O], output: O)


object InputParser {
    extension (line: String)
        def isCommand: Boolean = line.startsWith("$")

    def parse(input: String): List[CommandWithOutput[_]] = identifyChunks(List.empty, input.split("\n").toList).map { chunk =>
        val command = identifyCommand(chunk.head)
        CommandWithOutput(command, command.unapply(chunk.tail))
    }

    def identifyCommand(line: String): Command[_] =
        require(line.startsWith("$"), s"Line '$line' is not a command")
        val parts = line.substring(1).trim().split(" ")
        parts.head match
            case "ls" => ListDirectoryContents
            case "cd" =>
                require(parts.tail.size == 1, "You can only change into a single directory!")
                ChangeDirectory(parts.tail.head)
            case unknown => throw IllegalArgumentException(s"Unknown shell command '$unknown'")
    
    private def identifyChunks(result: List[List[String]], unindentified: List[String]): List[List[String]] =
        if (unindentified.isEmpty) result
        else 
            val nextChunk = unindentified.head +: unindentified.tail.takeWhile(!_.isCommand)
            identifyChunks(result :+ nextChunk, unindentified.tail.dropWhile(!_.isCommand))
}
