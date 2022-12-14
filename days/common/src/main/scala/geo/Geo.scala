package geo

import scala.math.Numeric
import better.files.*

import math.Numeric.Implicits.infixNumericOps
import scala.annotation.targetName

extension [N: Numeric](n: N)
    private def num = summon[Numeric[N]]
    def until(end: N): LazyList[N] = LazyList.iterate(n) { c => c + num.one }.takeWhile(num.lt(_, end))
    def to(end: N): LazyList[N] = until(end).appended(end)

case class Point2D[C: Numeric](x: C, y: C) {
    def between(that: Point2D[C]): Vector2D[C] =
        Vector2D(this.x - that.x, this.y - that.y)
    def manhattanDistance(that: Point2D[C]): C =
        (this.x - that.x).abs + (this.y - that.y).abs

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

    def andAllSurrounding: Set[Point2D[C]] =
        (edgeNeighbors ++ cornerNeighbors) + this

    override def toString: String = s"P($x/$y)"
    private def _0: C = summon[Numeric[C]].zero
    private def _1: C = summon[Numeric[C]].one
}
case class Rectangle[C: Numeric](p1: Point2D[C], p2: Point2D[C]) {
    def minMaxRectangle: Rectangle[C] = Rectangle.boundingBox(Seq(p1, p2))
    def points: Seq[Point2D[C]] = Seq(p1, p2)

    def enclosedPoints: Set[Point2D[C]] =
        val minMax = minMaxRectangle
        (for {
            x <- minMax.p1.x to minMax.p2.x
            y <- minMax.p1.y to minMax.p2.y
        } yield Point2D(x, y)).toSet
}

object Rectangle:
    def boundingBox[N: Numeric](ps: Iterable[Point2D[N]]): Rectangle[N] =
        val xs = ps.map(_.x)
        val ys = ps.map(_.y)
        Rectangle(Point2D(xs.min, ys.min), Point2D(xs.max, ys.max))

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
    export content.size

    def boundingBox: Rectangle[C] = Rectangle(
        Point2D[C](
            content.keys.view.map(_.x).min,
            content.keys.view.map(_.y).min
        ), Point2D[C](
            content.keys.view.map(_.x).max,
            content.keys.view.map(_.y).max
        ))

    def asString(valuePrinter: V => Char): String = asString(boundingBox, valuePrinter)
    def asString(bbox: Rectangle[C], valuePrinter: V => Char): String = {
        val Rectangle(min, max) = bbox.minMaxRectangle
        (min.y to max.y).map { y =>
            (min.x to max.x).map { x =>
                content.get(Point2D(x, y)).map(valuePrinter).getOrElse(' ')
            }.mkString
        }.mkString("\n")
    }
    def environmentAsString(valuePrinter: V => Char, ps: Point2D[C]*): String =
        asString(Rectangle.boundingBox(ps.flatMap(_.andAllSurrounding)), valuePrinter)

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