package geo

import scala.math.Numeric
import better.files.*

import math.Numeric.Implicits.infixNumericOps
import scala.annotation.targetName

case class Point2D[C: Numeric](x: C, y: C) {
    def between(that: Point2D[C]): Vector2D[C] =
        Vector2D(this.x - that.x, this.y - that.y)

    @targetName("plus")
    def +(that: Vector2D[C]): Point2D[C] =
        Point2D(this.x + that.x, this.y + that.y)

    def ray(vec: Vector2D[C]): LazyList[Point2D[C]] =
        LazyList.iterate(this) { p => p + vec }

    def edgeNeighbors: Set[Point2D[C]] = Set(
        Vector2D[C]( _0,  _1),
        Vector2D[C]( _0, -_1),
        Vector2D[C]( _1,  _0),
        Vector2D[C](-_1,  _0),
    ).map(this + _)

    def cornerNeighbors: Set[Point2D[C]] = Set(
        Vector2D[C](-_1, -_1),
        Vector2D[C]( _1, -_1),
        Vector2D[C](-_1,  _1),
        Vector2D[C]( _1,  _1),
    ).map(this + _)

    override def toString: String = s"P($x/$y)"
    private def _0: C = summon[Numeric[C]].zero
    private def _1: C = summon[Numeric[C]].one
}
case class Rectangle[C](p1: Point2D[C], p2: Point2D[C])

case class Vector2D[C: Numeric](x: C, y: C):
    @targetName("plus")
    def +(that: Vector2D[C]): Vector2D[C] =
        Vector2D(this.x + that.x, this.y + that.y)
    @targetName("minus")
    def -(that: Vector2D[C]): Vector2D[C] =
        Vector2D(this.x - that.x, this.y - that.y)
    def sign: Vector2D[C] = Vector2D(x.sign, y.sign)
    override def toString: String = s"V($x/$y)"

case class Map2D[C: Numeric, V](content: Map[Point2D[C], V]) {

    def boundingBox: Rectangle[C] = Rectangle(
        Point2D[C](
            content.keys.view.map(_.x).min,
            content.keys.view.map(_.y).min
        ), Point2D[C](
            content.keys.view.map(_.x).max,
            content.keys.view.map(_.y).max
        ))

    def crawl(start: Point2D[C], neighbors: Point2D[C] => Iterable[Point2D[C]], continue: V => Boolean): List[Point2D[C]] = {
        def crawlRec(left: List[Point2D[C]], visited: Set[Point2D[C]], result: List[Point2D[C]]): List[Point2D[C]] = left match {
            case Nil => result
            case head :: rest =>
                if (visited contains head) crawlRec(rest, visited, result) else {
                    val nexts = neighbors(head).filterNot(visited.contains).filter(p => content.get(p).exists(continue))
                    crawlRec(left ++ nexts, visited + head, result :+ head)
                }
        }
        crawlRec(List(start), Set.empty, List.empty)
    }

}

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

    extension[V] (map: Map2D[Int, V])
        def size: Int = map.content.size
        def asString(valuePrinter: V => Char): String = {
            val Rectangle(Point2D(minX, minY), Point2D(maxX, maxY)) = map.boundingBox
            (minY to maxY).map { y =>
                (minX to maxX).map(x => map.content.get(Point2D(x, y)).map(valuePrinter).getOrElse(' ')).mkString
            }.mkString("\n")
        }
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