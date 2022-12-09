import better.files.Resource
import geo.*

enum MoveDirection:
  case Left, Up, Right, Down
  def asVector: Vector2D[Int] = this match
    case Left  => Vector2D(-1,  0)
    case Up    => Vector2D( 0, -1)
    case Right => Vector2D( 1,  0)
    case Down  => Vector2D( 0,  1)

case class Move(direction: MoveDirection, distance: Int) {
  def asChainOfSingleSteps: List[MoveDirection] = List.fill(distance)(direction)
}
object Move {
  def parseLines(raw: String): Seq[Move] = raw.split("\n").map(fromLine)
  def fromLine(line: String): Move =
    val one :: two :: Nil = line.split(" ").toList : @unchecked
    val dir = one match
      case "R" => MoveDirection.Right
      case "U" => MoveDirection.Up
      case "L" => MoveDirection.Left
      case "D" => MoveDirection.Down
    Move(dir, two.toInt)
}

case class Rope(knots: List[Point2D[Int]], visited: Set[Point2D[Int]]) {
  require(knots.size >= 2, s"The rope needs to have to have at least 2 knots!")
  export knots.{head, last}
  extension (p: Point2D[Int])
    def neighbors: Set[Point2D[Int]] = p.edgeNeighbors ++ p.cornerNeighbors
    def isNextTo(p2: Point2D[Int]): Boolean = (p neighbors p2) || (p == p2)

  def move(m: Move): Rope = move(m.asChainOfSingleSteps*)
  def move(dirs: MoveDirection*): Rope = dirs.foldLeft(this) { _ step _ }
  def moves(ms: Seq[Move]): Rope = ms.foldLeft(this) { _ move _ }

  override def toString: String = s"Rope(${knots.map(p => s"(${p.x}/${p.y})").mkString(" # ")} | [${visited.toList.sortBy(p => (p.y, p.x)).map(p => s"(${p.y/p.x})")}])"
  private def step(dir: MoveDirection): Rope =
    val newKnots = knots.tail.scanLeft(knots.head + dir.asVector) { (ref, knot) =>
      if ref isNextTo knot then knot
      else knot + (ref between knot).sign
    }
    Rope(newKnots, visited + newKnots.last)
}

val origin: Point2D[Int] = Point2D(0, 0)
object Rope {
  def apply(knotCount: Int): Rope = apply(knotCount, origin)
  def apply(knotCount: Int, p: Point2D[Int]): Rope = new Rope(List.fill(knotCount)(p), Set(p))
}

val input: String = Resource.asString("inputA.txt").getOrElse(sys.error("Couldn't read input file"))

@main def part1 =
  val moves = Move.parseLines(input)
  val res = Rope(2).moves(moves)
  println(s"The tail visited ${res.visited.size} places")

@main def part2 =
  val moves = Move.parseLines(input)
  val res = Rope(10).moves(moves)
  println(s"The tail visited ${res.visited.size} places")