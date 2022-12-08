import scala.math.Numeric
import better.files.*

package geo {
    import math.Numeric.Implicits.infixNumericOps

    case class Point2D[C](x: C, y: C)
    case class Rectangle[C](p1: Point2D[C], p2: Point2D[C])
    case class Vector2D[C](x: C, y: C)
    case class Map2D[C, V](content: Map[Point2D[C], V])

    object Map2D {
        def contentFrom[T](content: String, charInterpreter: Char => T): Map[Point2D[Int], T] =
            (for {
                (line, y) <- content.linesIterator.zipWithIndex
                (char, x) <- line.zipWithIndex
            } yield Point2D[Int](x, y) -> charInterpreter(char)
        ).toMap

        def contentFromRessource[T](resourceName: String, charInterpreter: Char => T): Map[Point2D[Int], T] =
            contentFrom(Resource.getAsString(resourceName), charInterpreter)

        def fromResource[T, V](resourceName: String, charInterpreter: Char => T, typeInit: Map[Point2D[Int], T] => V): V = typeInit(contentFromRessource(resourceName, charInterpreter))
        def fromResource[T](resourceName: String, charInterpreter: Char => T): Map2D[Int, T] = fromResource(resourceName, charInterpreter, Map2D.apply)
    }

    extension [C: Numeric](p: Point2D[C])
        def between(other: Point2D[C]): Vector2D[C] =
            Vector2D(p.x - other.x, p.y - other.y)
        def add(vec: Vector2D[C]): Point2D[C] =
            Point2D(p.x + vec.x, p.y + vec.y)
        def +(vec: Vector2D[C]): Point2D[C] = add(vec)
        def edgeNeighbors: Set[Point2D[C]] = {
            val num = summon[Numeric[C]]
            Set(
                Vector2D[C](num.zero, num.one),
                Vector2D[C](num.zero, -num.one),
                Vector2D[C](num.one, num.zero),
                Vector2D[C](-num.one, num.zero)
            ).map(p + _)
        }

    extension [C: Numeric](v: Vector2D[C])
        def add(other: Vector2D[C]): Vector2D[C] =
            Vector2D(v.x + other.x, v.y + other.y)
        def +(other: Vector2D[C]): Vector2D[C] = add(other)
        def minus(other: Vector2D[C]): Vector2D[C] =
            Vector2D(v.x - other.x, v.y - other.y)
        def -(other: Vector2D[C]): Vector2D[C] = minus(other)
    
    extension (rect: Rectangle[Int])
        def enclosedCoordinates: Set[Point2D[Int]] = (for {
            y <- Math.min(rect.p1.y, rect.p2.y) to Math.max(rect.p1.y, rect.p2.y)
            x <- Math.min(rect.p1.x, rect.p2.x) to Math.max(rect.p1.x, rect.p2.x)
        } yield Point2D[Int](x, y)).toSet

    extension [N: Numeric, V](maze: Map2D[N, V])
        def boundingBoxMin: Point2D[N] = Point2D[N](
            maze.content.keys.view.map(_.x).min,
            maze.content.keys.view.map(_.y).min
        )
        def boundingBoxMax: Point2D[N] = Point2D[N](
            maze.content.keys.view.map(_.x).max,
            maze.content.keys.view.map(_.y).max   
        )
        def boundingBox: Rectangle[N] =
            Rectangle(boundingBoxMin, boundingBoxMax)
        def crawl(start: Point2D[N], neighbors: Point2D[N] => Iterable[Point2D[N]], continue: V => Boolean): List[Point2D[N]] = {
            def crawlRec(left: List[Point2D[N]], visited: Set[Point2D[N]], result: List[Point2D[N]]): List[Point2D[N]] = left match {
                case Nil => result
                case head :: rest =>
                    if (visited contains head) crawlRec(rest, visited, result) else {
                        val nexts = neighbors(head).filterNot(visited.contains).filter(p => maze.content.get(p).exists(continue))
                        println(s"$head next ${nexts.mkString(", ")}")
                        crawlRec(left ++ nexts, visited + head, result :+ head)
                    } 
            }
            crawlRec(List(start), Set.empty, List.empty)
        }

    extension [V](map: Map2D[Int, V])
        def size: Int = map.content.size
        def asString(valuePrinter: V => Char): String = {
            val Point2D[Int](minX, minY) = map.boundingBoxMin
            val Point2D[Int](maxX, maxY) = map.boundingBoxMax
            (minY to maxY).map { y =>
                (minX to maxX).map( x => map.content.get(Point2D(x,y)).map(valuePrinter).getOrElse(' ')).mkString
            }.mkString("\n")
        }



    case class Point3D[C](x: C, y: C, z: C)
    case class Vector3D[C](x: C, y: C, z: C)
    case class Map3D[C, V](content: Map3D[Point3D[C], V])

    extension [C: Numeric](p: Point3D[C])
        def between(other: Point3D[C]): Vector3D[C] =
            Vector3D(p.x - other.x, p.y - other.y, p.z - other.z)
        def add(vec: Vector3D[C]): Point3D[C] =
            Point3D(p.x + vec.x, p.y + vec.y, p.z + vec.z)
        def +(vec: Vector3D[C]): Point3D[C] = add(vec)

    extension [C: Numeric](v: Vector3D[C])
        def add(other: Vector3D[C]): Vector3D[C] =
            Vector3D(v.x + other.x, v.y + other.y, v.z + other.z)
        def +(other: Vector3D[C]): Vector3D[C] = add(other)
        def minus(other: Vector3D[C]): Vector3D[C] =
            Vector3D(v.x - other.x, v.y - other.y, v.z + other.z)
        def -(other: Vector3D[C]): Vector3D[C] = minus(other)
}