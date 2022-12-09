import munit.FunSuite
import better.files.Resource

object Example {
    val i = file("a/e/i", 584)
    val f = file("a/f", 29116)
    val g = file("a/g", 2557)
    val h_lst = file("a/h.lst", 62596)
    val e = dir("a/e", i)
    val a = dir("a", e, f, g, h_lst)
    val b_txt = file("b.txt", 14848514)
    val c_dat = file("c.dat", 8504156)

    val j = file("d/j", 4060174)   
    val d_log = file("d/d.log", 8033020)   
    val d_ext = file("d/d.ext", 5626152)   
    val k = file("d/k", 7214296)   
    val d = dir("d", j, d_log, d_ext, k)

    val root = Directory(File.rootPath, List(a, b_txt, c_dat, d))

    def file(name: String, size: Int): DataFile = DataFile(File.expectPath(name), BigInt(size))
    def dir(name: String, files: File*): Directory = Directory(File.expectPath(name), files.toList)
}

class TestShell extends FunSuite {
    import Example.{dir, file}
    val example = Resource.asString("example.txt").getOrElse(throw new IllegalStateException("Couldn't read example file"))
    val exampleShell = Shell.fromRoot(Example.root)

    def checkPwd(name: String, mod: Shell => Shell, expectedFull: String)(using loc: munit.Location): Unit = test(name) {
        assertEquals(mod(exampleShell).pwd.fullPath, expectedFull)
    }

    checkPwd("Change directory works: / -> /a", _.cd("a"), "/a")
    checkPwd("Change directory works: / -> /a/e", _.cd("a/e"), "/a/e")
    checkPwd("Change directory works: / -> /a -> /a/e", _.cd("a").cd("e"), "/a/e")

    test("Change directory into unknown file does not work") {
        intercept[RuntimeException] { exampleShell.cd("doesNotExist") }
    }

    test("Go up to a parent: /a -> /") {
        assertEquals(exampleShell.cd("a").cd("..").pwd.fullPath, "/") 
    }

    test("Directory size for 'e'") {
        assertEquals(exampleShell.cd("a/e").file.size, BigInt(584))
    }

    test("Directory size for 'a'") {
        assertEquals(exampleShell.cd("a").file.size, BigInt(94853))
    }

    test("Directory size for 'd'") {
        assertEquals(exampleShell.cd("d").file.size, BigInt(24933642))
    }

    test("Directory size for '/'") {
        assertEquals(exampleShell.file.size, BigInt(48381165))
    }

    test("Add a few files to a/e") {
        val res = exampleShell.cd("a/e").adapt(CommandWithOutput(ListDirectoryContents, List(
            file("./1", 1),
            file("./2", 2),
        )))
        val filepaths = res.pwdAs[Directory].files.map(_.fullPath)
        assertEquals(filepaths, List("/a/e/i", "/a/e/1", "/a/e/2"))
        assertEquals(res.pwdAs[Directory].size, BigInt(587)) // +3
        assertEquals(res.root.size, BigInt(48381168))       // +3
    }

    test("Example works out in right working directory") {
        val result = Shell.newSystem.adapt(InputParser.parse(example))
        assertEquals(result.pwd.fullPath, "/d")
    }

    test("Example contains the right sizes ") {
        val result = Shell.newSystem.adapt(InputParser.parse(example)).cd("/")
        assertEquals(result.root.size, BigInt(48381165))
        assertEquals(result.totalUsedSize, BigInt(48381165))
    }

    test("Example can be filtered correctly ") {
        val result = Shell.newSystem.adapt(InputParser.parse(example))
        val relevantSizes = result.root.filesRecursively.collect {
            case dir: Directory if dir.size <= 100000 => dir.size 
        }
        assertEquals(relevantSizes.sum, BigInt(95437))
    }

    test("Example can be filtered correctly ") {
        val result = Shell.newSystem.adapt(InputParser.parse(example))
        val relevantSizes = result.root.filesRecursively.collect {
            case dir: Directory if dir.size <= 100000 => dir.size 
        }
        assertEquals(relevantSizes.sum, BigInt(95437))
    }

    test("Example finds the right directories ") {
        val result = Shell.newSystem.adapt(InputParser.parse(example))
        assertEquals(result.totalUsedSize, BigInt(48381165))
    }

}
