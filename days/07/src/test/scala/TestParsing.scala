import munit.FunSuite
import better.files.Resource
import java.nio.file.Path

class TestParsing extends FunSuite {

    test("Changing into a directory can be parsed") {
        assertEquals(InputParser.parse("$ cd /").head, CommandWithOutput(ChangeDirectory("/"), ()))
        assertEquals(InputParser.parse("$ cd a").head, CommandWithOutput(ChangeDirectory("a"), ()))
    }
  
    test("Listing a directory can be parsed") {
        val text = """|$ ls
                      |dir a
                      |14848514 b.txt
                      |8504156 c.dat
                      |dir d""".stripMargin
        val command = InputParser.parse(text).head
        val expected = CommandWithOutput(ListDirectoryContents, List(
            dir("./a"),
            file("./b.txt", 14848514),
            file("./c.dat", 8504156),
            dir("./d"),
        ))
        assertEquals(command, expected)
    }

    private def file(name: String, size: Int): DataFile = DataFile(File.expectPath(name), BigInt(size))
    private def dir(name: String, files: File*): Directory = Directory(File.expectPath(name), files.toList)
}
