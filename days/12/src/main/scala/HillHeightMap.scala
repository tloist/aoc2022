import better.files.Resource
import geo.*

import scala.collection.mutable

object HillHeightMap {
  def read(input: String): HillHeightMap = HillHeightMap(Map2D.contentFrom(input, identity))
}

class HillHeightMap(content: Map[Point2D[Int], Char]) extends Map2D[Int, Char](content) {
  require(content.values.map(_.toString).forall("[a-zSE]".r.matches), s"Map contains illegal characters!")
  require(content.values.count(_ == 'S') == 1, s"Map contains multiple starting points")
  require(content.values.count(_ == 'E') == 1, s"Map contains multiple exit points")

  extension (p: Point2D[Int])
    def reachableNeighbors: Set[Point2D[Int]] = heightOf(p).map { ownHeight =>
      p.edgeNeighbors.filter(heightOf(_).exists(_ <= ownHeight + 1))
    }.getOrElse(Set.empty)

  def searchPath(
    heuristic: Point2D[Int] => Int,
    travelCost: (Point2D[Int], Point2D[Int]) => Int
  ): Either[String, List[Point2D[Int]]] =
    case class Open(point: Point2D[Int], costG: Int, costH: Int) {
      val costF: Int = costG + costH
      override def toString: String = s"(${point.x}/${point.y}):$costF"
    }
    given Ordering[Open] = Ordering.by(_.costF)
    case class ParentInfo(prev: Point2D[Int], cost: Int)
    val explored = scala.collection.mutable.Map.empty[Point2D[Int], ParentInfo]
    def reconstructPath(p: Point2D[Int]): List[Point2D[Int]] = LazyList.unfold(p) { c =>
      explored.get(c).map(_.prev).filterNot(_ == start).map(x => (x, x))
    }.prepended(p).toList

    val open: mutable.PriorityQueue[Open] = scala.collection.mutable.PriorityQueue.empty[Open]
    open enqueue Open(start, 0, heuristic(start))
    val closed = scala.collection.mutable.Set.empty[Point2D[Int]]

    while (open.nonEmpty) {
//      print("Next inspecting ")
//      print(open.headOption.map(_.toString).getOrElse("---"))
//      print(if open.size == 1 then " single option " else s" out of ${open.size} options")
//      println(if open.size > 1 then open.maxOption.map(m => s". Worst option: ${m.costF}").getOrElse("") else "")
      val current = open.dequeue()
      closed.add(current.point)
      if current.point == exit then return Right(reconstructPath(current.point))

      current.point.reachableNeighbors.diff(closed).foreach { next =>
        val nextCostG = travelCost(current.point, next)
        if (!explored.contains(next) || explored(next).cost > nextCostG) {
          explored.put(next, ParentInfo(current.point, nextCostG))
          open enqueue Open(next, nextCostG, heuristic(next))
        }
      }
    }
    None
    Left(s"Failed to find path after visiting ${closed.size} nodes!")

  def searchPath: Either[String, List[Point2D[Int]]] =
    searchPath(_.manhattanDistance(exit), (_, _) => 1)

  def findExit(): List[Point2D[Int]] = findExit(start)

  def findExit(theStart: Point2D[Int]): List[Point2D[Int]] = {
    val parents = scala.collection.mutable.Map.empty[Point2D[Int], Point2D[Int]]
    val queue = scala.collection.mutable.Queue(theStart)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if current == exit then return LazyList.unfold(current) { p => parents.get(p).filterNot(_ == theStart).map(x => (x, x)) }.prepended(start).toList
      current.reachableNeighbors.diff(parents.keySet).foreach { next =>
        queue enqueue next
        parents.put(next, current)
      }
    }
    List()
  }

  val start: Point2D[Int] = content.find(_._2 == 'S').map(_._1).get
  val exit: Point2D[Int] = content.find(_._2 == 'E').map(_._1).get
  def heightOf(p: Point2D[Int]): Option[Int] = content.get(p).map(heightOf)
  def heightOf(value: Char): Int = value match
    case 'S' => heightOf('a')
    case 'E' => heightOf('z')
    case _ => value.toInt - 96
}

@main def part1 =
  val exampleMap = HillHeightMap.read(Resource.getAsString("inputA.txt"))
  val path = exampleMap.findExit()
  println(path)
  println(path.size)

@main def part2 =
  val exampleMap = HillHeightMap.read(Resource.getAsString("inputA.txt"))
  val startingPoints = exampleMap.content.filter(_._2 == 'a').keys
  val result = startingPoints.map { p =>
    exampleMap.findExit(p).size
  }.filter(_ > 0).min
  println()
  println(s"The best starting point is $result")