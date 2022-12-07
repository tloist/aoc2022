import scala.util.matching.Regex

trait Command[O] {
    def unapply(output: List[String]): O
}

case class ChangeDirectory(name: String) extends Command[Unit] {
    def unapply(output: List[String]): Unit = require(output.isEmpty, s"Change of directory shouldn't produce an output, but did: $output")
}
case object ListDirectoryContents extends Command[List[File]] {
    def unapply(output: List[String]): List[File] = output.map(parseOutputline)

    private def parseOutputline(line: String): File = line match
        case directoryOutput(name) => Directory(File.relativePath(name), List.empty)
        case fileOutput(size, name) => DataFile(File.relativePath(name), BigInt(size))

    private val directoryOutput: Regex = """dir (\S+)""".r
    private val fileOutput: Regex = """(\d+) (\S+)""".r
}