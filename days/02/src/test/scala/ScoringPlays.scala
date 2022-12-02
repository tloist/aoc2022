import munit.FunSuite
import Play.*

class ScoringPlays extends FunSuite {

    test("Score Line 1 in Part A") {
        assertEquals(score(List((Rock, Paper))), 8)
    }

    test("Score Line 2 in Part A") {
        assertEquals(score(List((Paper, Rock))), 1)
    }

    test("Score Line 3 in Part A") {
        assertEquals(score(List((Scissor, Scissor))), 6)
    }

    test("Score Example in Part A") {
        assertEquals(score(example), 15)
    }

}
