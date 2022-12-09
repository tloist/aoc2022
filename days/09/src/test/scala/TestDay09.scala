import munit.FunSuite
import geo._
import MoveDirection._

class TestDay09 extends FunSuite {
  val rope2: Rope = Rope(2)

  test("Blank rope, everything starts at (0,0)") {
    assertEquals(rope2.head, origin)
    assertEquals(rope2.last, origin)
    assertEquals(rope2.visited, Set(origin))
  }

  test("Move to right: Tail follows head") {
    val res = rope2.move(Move(Right, 2))
    assertEquals(res.head, Point2D(2, 0))
    assertEquals(res.last, Point2D(1, 0))
    assertEquals(res.visited, Set(origin, Point2D(1,0)))
  }

  test("Move to left: Tail follows head") {
    val res = rope2.move(Move(Left, 2))
    assertEquals(res.head, Point2D(-2, 0))
    assertEquals(res.last, Point2D(-1, 0))
    assertEquals(res.visited, Set(origin, Point2D(-1, 0)))
  }

  test("Move up: Tail follows head") {
    val res = rope2.move(Move(Up, 2))
    assertEquals(res.head, Point2D(0, -2))
    assertEquals(res.last, Point2D(0, -1))
    assertEquals(res.visited, Set(origin, Point2D(0, -1)))
  }

  test("Move down: Tail follows head") {
    val res = rope2.move(Move(Down, 2))
    assertEquals(res.head, Point2D(0, 2))
    assertEquals(res.last, Point2D(0, 1))
    assertEquals(res.visited, Set(origin, Point2D(0, 1)))
  }

  def diogonalExample: Rope = rope2.move(Up, Right)
  test("Diagonal example: Up Movement") {
    val res = diogonalExample.move(Up)
    assertEquals(res.head, Point2D(1, -2))
    assertEquals(res.last, Point2D(1, -1))
    assertEquals(res.visited, Set(origin, Point2D(1, -1)))
  }

  test("Diagonal example: Up Movement") {
    val res = diogonalExample.move(Right)
    assertEquals(res.head, Point2D(2, -1))
    assertEquals(res.last, Point2D(1, -1))
    assertEquals(res.visited, Set(origin, Point2D(1, -1)))
  }

  test("Head and Tail on the same spot") {
    val res = rope2.move(Right, Left)
    assertEquals(res.head, origin)
    assertEquals(res.last, origin)
    assertEquals(res.visited, Set(origin))
  }

  test("Example: Tail visited 13 positions") {
    val exampleLines =
      """|R 4
         |U 4
         |L 3
         |D 1
         |R 4
         |D 1
         |L 5
         |R 2""".stripMargin
    val moves = Move.parseLines(exampleLines)
    val res = rope2.moves(moves)
    assertEquals(res.visited.size, 13)
  }

  test("Larger Example: Tail visited 36 positions") {
    val rope10: Rope = Rope(10)
    val exampleLines =
      """|R 5
         |U 8
         |L 8
         |D 3
         |R 17
         |D 10
         |L 25
         |U 20""".stripMargin
    val moves = Move.parseLines(exampleLines)
    val res = rope10.moves(moves)
    assertEquals(res.visited.size, 36)
  }
}
