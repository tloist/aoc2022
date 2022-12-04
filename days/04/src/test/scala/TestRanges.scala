import munit.FunSuite

class TestRanges extends FunSuite {

    test("Parse example") {
        val example = readInput("""|2-4,6-8
                                   |2-3,4-5
                                   |5-7,7-9
                                   |2-8,3-7
                                   |6-6,4-6
                                   |2-6,4-8""".stripMargin)
        assertEquals(example, List(
            (Range(2,4), Range(6,8)),
            (Range(2,3), Range(4,5)),
            (Range(5,7), Range(7,9)),
            (Range(2,8), Range(3,7)),
            (Range(6,6), Range(4,6)),
            (Range(2,6), Range(4,8)),
        ))
    }

    test("Example line 1") {
        val (first, second) = (Range(2,4), Range(6,8))
        assertEquals(first fullyContains second, false)
        assertEquals(first isSeparate second, true)
        assertEquals(first overlaps second, false)
    }

    test("Example line 2") {
        val (first, second) = (Range(2,3), Range(4,5))
        assertEquals(first fullyContains second, false)
        assertEquals(first isSeparate second, true)
        assertEquals(first overlaps second, false)
    }

    test("Example line 3") {
        val (first, second) = (Range(5,7), Range(7,9))
        assertEquals(first fullyContains second, false)
        assertEquals(first isSeparate second, false)
        assertEquals(first overlaps second, true)
    }

    test("Example line 4") {
        val (first, second) = (Range(2,8), Range(3,7))
        assertEquals(first fullyContains second,  true)
        assertEquals(first isSeparate second, false)
        assertEquals(first overlaps second, true)
    }

}
