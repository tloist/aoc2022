import munit.FunSuite
import better.files.Resource
import geo._

class ExampleTest extends FunSuite {
  def exampleInput: String = Resource.getAsString("example.txt")
  def exampleCave: Cave = Cave.parse(exampleInput, Point2D(500, 0))

  test("Example last sand index") {
    val lastIndex = exampleCave.sandSteps.zipWithIndex.map(_._2).last
    assertEquals(lastIndex, 23) // First cave has index 0
  }

  test("Example sand positions") {
    val lastCave = exampleCave.sandSteps.last
    val sandPositions = lastCave.map.content.collect { case (p, Field.Sand) => p }.toSet
    val expectedSand = Set(
                                                             p(500, 2),
                                                  p(499, 3), p(500, 3), p(501, 3),
                                       /*  #  */  p(499, 4), p(500, 4), p(501, 4), /*  #  */
                            p(497, 5), /*  #  */  p(499, 5), p(500, 5), p(501, 5), /*  #  */
                 /*  #          #          #  */  p(499, 6), p(500, 6), p(501, 6), /*  #  */
                                       p(498, 7), p(499, 7), p(500, 7), p(501, 7), /*  #  */
      p(495, 8),            p(497, 8), p(498, 8), p(499, 8), p(500, 8), p(501, 8), /*  #  */
      /*  #          #          #          #          #          #          #           # */
    )
    assertEquals(sandPositions, expectedSand)
  }

  test("Part B: Example sand positions") {
    val lastCave = exampleCave.withImplicitFloor(2).sandSteps.last
    val expected =
      """|............o............
         |...........ooo...........
         |..........ooooo..........
         |.........ooooooo.........
         |........oo#ooo##o........
         |.......ooo#ooo#ooo.......
         |......oo###ooo#oooo......
         |.....oooo.oooo#ooooo.....
         |....oooooooooo#oooooo....
         |...ooo#########ooooooo...
         |..ooooo.......ooooooooo..
         |#########################""".stripMargin
    assertEquals(lastCave.asString, expected)
  }

  def p(x: Int, y: Int): Point2D[Int] = Point2D(x, y)

}
