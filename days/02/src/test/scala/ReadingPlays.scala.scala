import munit.FunSuite
import Play.*

class ReadingPlays extends FunSuite {

    test("Reading input as 2 plays in Part A") {
        val read = readPlays("""|A Y
                                |B X
                                |C Z""".stripMargin)
        assertEquals(read, example)
    }

}
