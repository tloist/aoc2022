import Cave.parseLine
import better.files.Resource
import geo.*

enum Field(val char: Char):
  case Air        extends Field('.')
  case Rock       extends Field('#')
  case SandSource extends Field('+')
  case Sand       extends Field('o')

def linePoints(from: Point2D[Int], to: Point2D[Int]): Set[Point2D[Int]] =
  require(from.x == to.x || from.y == to.y, s"Points $from and $to are not parallel to one of the axis!")
  to.ray((from between to).sign).takeWhile(_ != from).toSet ++ Seq(to, from)

case class Cave(step: BigInt, map: Map2D[Int, Field], sandSource: Point2D[Int]) {
  def asString: String = map.asString(_.char)
  def environmentAsString(ps: Point2D[Int]*): String = map.environmentAsString(_.char, ps*)

  def withFields(fieldtype: Field, on: Iterable[Point2D[Int]]): Cave =
    val newContent = on.foldLeft(map.content) { (m, p) => m.updated(p, fieldtype) }
    val newAir = Rectangle.boundingBox(newContent.keys).enclosedPoints.filterNot(newContent.contains)
    val filled = newAir.foldLeft(newContent) { (m, p) => m.updated(p, Field.Air) }
    copy(map = Map2D(filled))

  def withImplicitFloor(distance: Int): Cave =
    val Rectangle(prevMin, prevMax) = map.boundingBox
    val maxY = prevMax.y + distance
    val totalDepth = maxY - prevMin.y
    val minXByDepth = sandSource.x - (totalDepth + 1)
    val minX = Math.min(prevMin.x, minXByDepth)
    val maxXByDepth = sandSource.x + (totalDepth + 1)
    val maxX = Math.max(prevMax.x, maxXByDepth)

    val line = linePoints(Point2D(minX, maxY), Point2D(maxX, maxY))
    withFields(Field.Rock, line)

  def fillWithSand(on: Point2D[Int]): Cave =
    require(map.content.get(on).exists(freeForSand.contains), s"Field $on cannot be filled with Sand!")
    // As above implicitly means, that the field is within our currently known fields: No need to add Air here
    copy(map = Map2D(map.content.updated(on, Field.Sand)))

  def sandStep: Cave =
    def isFreeForSand(p: Point2D[Int]): Boolean = map.content.get(p).exists(freeForSand.contains)
    def isStopForSand(p: Point2D[Int]): Boolean = sandVectors.forall(v => map.content.get(p + v).exists(solidGround.contains))
    def newSandPosition(p: Point2D[Int]): Option[Point2D[Int]] =
      if !isFreeForSand(p) then None
      else if isStopForSand(p) then Some(p)
      else sandVectors.map(p + _).find(isFreeForSand).flatMap(newSandPosition)
    newSandPosition(sandSource).map(fillWithSand).getOrElse(this)

  def sandSteps: LazyList[Cave] = LazyList.unfold(this) { c => Option(c.sandStep).filter(_.map != c.map).map(x => (x, x)) }

  private val solidGround = Set(Field.Rock, Field.Sand)
  private val freeForSand = Set(Field.Air, Field.SandSource)
  private val sandVectors = Seq(Vector2D(0, 1), Vector2D(-1, 1), Vector2D(1,1))
}

object Cave {
  def parse(input: String, source: Point2D[Int]): Cave =
    val lines = input.split("\n").map(parseLine)
    val rocks = lines.flatMap { line =>
      if line.isEmpty then Set.empty
      else if line.size == 1 then Set(line.head)
      else line.tail.foldLeft((Set(line.head), line.head)) { (acc, p) =>
        val (points, from) = acc
        (points ++ linePoints(from, p), p)
      }._1
    }.toSet
    require(!rocks.contains(source), s"Illegal input: Sand source is on a rock!")
    emptyCave(Rectangle.boundingBox(lines.flatten), source)
      .withFields(Field.Rock, rocks)
      .withFields(Field.SandSource, Seq(source))

  private def emptyCave(rectangle: Rectangle[Int], source: Point2D[Int]): Cave =
    val bbox = Rectangle.boundingBox(Seq(rectangle.p1, rectangle.p2, source))
    Cave(BigInt(0), Map2D(bbox.enclosedPoints.map(_ -> Field.Air).toMap), source)

  private def parseLine(line: String): List[Point2D[Int]] = line.split(" -> ").map(_.split(",")).map { pointParts =>
    require(pointParts.length == 2, s"These parts doesn't describe a point in 2D: ${pointParts.mkString(", ")}")
    Point2D(pointParts(0).toInt, pointParts(1).toInt)
  }.toList

}

def input = Resource.getAsString("inputA.txt")

def simulateSandFalling(cave: Cave): Int =
  cave.sandSteps.zipWithIndex.map(_._2).last + 1

@main def part1 =
  val i = simulateSandFalling(Cave.parse(input, Point2D(500, 0)))
  println(s"The last sand drops at: $i")

@main def part2 =
  val i = simulateSandFalling(Cave.parse(input, Point2D(500, 0)).withImplicitFloor(2))
  println(s"The last sand drops at: $i")

