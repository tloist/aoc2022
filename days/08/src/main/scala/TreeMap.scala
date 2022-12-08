import geo._
import javax.print.attribute.standard.PresentationDirection

type Point = geo.Point2D[Int]
type Vector = geo.Vector2D[Int]
def vector(x: Int, y: Int): Vector = Vector2D(x, y)

enum Direction:
    case North, East, South, West
    def asVector: Vector = this match
        case North => vector(0, -1)
        case East => vector(1, 0)
        case South => vector(0, 1)
        case West => vector(-1, 0)

extension [T](list: LazyList[T])
    def takeUntil(predicate: T => Boolean): LazyList[T] =
        val (head, tail) = list.span(predicate)
        tail.headOption.map(head :+ _).getOrElse(head)
     

class TreeMap(content: Map[Point, Int]) extends Map2D[Int, Int](content) {
   
    def isVisible(p: Point): Boolean =
        val ownHeight = content(p)
        val atTheEdge = p.edgeNeighbors.count(content.contains) < 4
        val allTreesLower = Direction.values.exists { dir => rayFrom(p, dir).map(content).forall(_ < ownHeight) }
        atTheEdge || allTreesLower
    def isInvisible(p: Point): Boolean = !isVisible(p)

    def scenicScore(p: Point): Int =
        val ownHeight = content(p)
        Direction.values.map { dir =>
            rayFrom(p, dir).map(content).takeUntil(_ < ownHeight).size
        }.product

    def visibleTreeLocations: Set[Point] = content.keySet.filter(isVisible)
    def neighborHeightsFor(p: Point): List[Int] = 
        p.edgeNeighbors.toList.filter(content.contains).map(content)

    def rayFrom(p: Point, dir: Direction): LazyList[Point] =
        LazyList.unfold(p) { cur => Option(cur.add(dir.asVector)).filter(content.contains).map(x => (x, x)) }
}

object TreeMap {
    def apply(content: Map[Point, Int]): TreeMap = new TreeMap(content)
    def fromResource(resourceName: String): TreeMap = Map2D.fromResource(resourceName, _.toString.toInt, TreeMap.apply)
}
