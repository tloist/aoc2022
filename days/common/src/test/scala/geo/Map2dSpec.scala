package geo

import munit.FunSuite
import java.nio.file.Paths

enum MapTile:
    case Free, Wall

class Map2DSpec extends FunSuite {
    val mapping = List( (MapTile.Free, '.'), (MapTile.Wall, '#') )
    val tileAsChar: MapTile => Char = t => mapping.find(t == _._1).map(_._2).getOrElse(' ')
    val interpretChar: Char => MapTile = c => mapping.find(c == _._2).map(_._1).getOrElse(
        throw IllegalArgumentException(s"Illegal character found: '$c'")
    )

    test("Basically parsing a rudimentary 2D maze") {
        val maze = Map2D.fromResource("testdata/map2D/example.txt", interpretChar)
        List(
            List(0, 4, 7, 8, 12, 13, 16, 18).map(Point2D(_, 0)),
            List(2, 6, 7, 11, 13, 15).map(Point2D(_, 1)),
            List(0, 1, 6, 8, 9, 10, 16, 17).map(Point2D(_, 2)),
            List(0, 5, 11, 12, 13, 15, 17).map(Point2D(_, 3)),
        ).flatten.foreach { pos => assertEquals(maze.content.get(pos), Some(MapTile.Wall), s"Not a wall at $pos")}
        val freePositions = List(
            List(1, 2, 3, 5, 6, 9, 10, 11, 14, 15, 17).map(Point2D(_, 0)),
            List(0, 1, 3, 4, 5, 8, 9, 10, 12, 14, 16, 17, 18).map(Point2D(_, 1)),
            List(2, 3, 4, 5, 7, 11, 12, 13, 14, 15, 18).map(Point2D(_, 2)),
            List(1, 2, 3, 4, 6, 7, 8, 9, 10, 14, 16, 18).map(Point2D(_, 3)),
        ).flatten.foreach { pos => assertEquals(maze.content.get(pos), Some(MapTile.Free), s"Not free at $pos")}
    }

    test("Crawling through 2D maze to identify an area") {
        val maze = Map2D.fromResource("testdata/map2D/example.txt", interpretChar)
        val area = maze.crawl(Point2D(5, 2), _.edgeNeighbors, MapTile.Free.equals).toSet
        assertEquals(area, Set(
            List(1, 2, 3, 5, 6).map(Point2D(_, 0)),
            List(0, 1, 3, 4, 5).map(Point2D(_, 1)),
            List(2, 3, 4, 5).map(Point2D(_, 2)),
            List(1, 2, 3, 4).map(Point2D(_, 3)),
        ).flatten)
    }

}