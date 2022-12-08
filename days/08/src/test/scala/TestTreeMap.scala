import munit.FunSuite
import better.files.Resource
import geo._

val example: TreeMap = TreeMap.fromResource("example.txt")

class TestTreeMap extends FunSuite {

    test("Example: Is parsed correctly") {
        val expected = """|30373
                          |25512
                          |65332
                          |33549
                          |35390""".stripMargin
        assertEquals(example.asString(_.toString.head), expected)
    }

    def checkVisiblity(name: String, p: Point, expectedVisibility: Boolean)(using loc: munit.Location): Unit = test(name) {
        if (!example.isVisible(p) == expectedVisibility) {
            val ownHeight = example.content(p)
            Console.err.println()
            Console.err.println(s"Failure at $name")
            Console.err.println(s"Tree at $p has height: $ownHeight")
            Console.err.println(s"Neighbors for tree at $p have height: ${example.neighborHeightsFor(p)}")
            for {
                dir <- Direction.values
                ray = rayFromTo(p, dir)
                allLower = ray.forall(_ <= ownHeight)
            } { println(s"Ray from $p $dir: ${rayFromTo(p, dir).mkString(", ")} means all are lower: $allLower") }
            Console.err.println(s"Tree $p is visible: ${example.isVisible(p)}")
            Console.err.println("---")
        }
        assertEquals(example.isVisible(p), expectedVisibility)
    }

    checkVisiblity("Example: Top left 5 is visible", point(1,1), true)
    checkVisiblity("Example: Top-Middle 5 is not visible", point(2,1), true)
    checkVisiblity("Example: Top-Right 1 is not visible", point(3,1), false)
    checkVisiblity("Example: Left-Middle 5 is visible", point(1,2), true)
    checkVisiblity("Example: Center 3 is not visible", point(2,2), false)
    checkVisiblity("Example: Right-Middle 3 is visible", point(3,2), true)
    checkVisiblity("Example: Bittom-Left 3 is not visible", point(1,3), false)
    checkVisiblity("Example: Bittom-Mddle 5 is visible", point(2,3), true)
    checkVisiblity("Example: Bittom-Right 4 is not visible", point(3,3), false)

    test("The number of visible trees") {
      assertEquals(example.visibleTreeLocations.size, 21)
    }

    test("Part B: Scenic Score of Middle-Upper 5") {
      assertEquals(example.scenicScore(point(2, 1)), 4)
    }

    test("Part B: Scenic Score of Middle-Bottom 5") {
      assertEquals(example.scenicScore(point(2, 3)), 8)
    }

    private def rayFromTo(from: Point, to: Direction): List[Int] = example.rayFrom(from, to).map(example.content).toList
    private def point(x: Int, y: Int): Point = Point2D(x, y)
}
