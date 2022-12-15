import Band.empty
import geo.*
import better.files.Resource

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex

/** A band of values, inclusive of the first but exclusive on the last */
// Basically a Range, but there is already an existing Scala collection that is more general (e.g. includes infinite ranges)
case class Band(from: BigInt, to: BigInt) {
  require(from <= to, s"This is not a legal band: $this")
  def size: BigInt = to - from
  def contains(n: BigInt): Boolean = n >= from && n < to
  def overlaps(that: Band): Boolean = this.to >= that.from || this.from > that.to
  // A union of non-overlapping bands cannot be represented within this data type: thus optional
  def union(that: Band): Option[Band] =
    if this overlaps that then Some(Band(min(this.from, that.from), max(this.to, that.to)))
    else None
  def intersect(that: Band): Band =
    val from = max(this.from, that.from)
    val to = min(this.to, that. to)
    if from < to then Band(from, to) else empty
  def diff(others: Iterable[Band]): List[Band] =
    Band.unionAll(others).map(_ intersect this).sliding(2).map { bs => Band(bs(0).to, bs(1).from)}.toList
  def isEmpty: Boolean = size == 0
  def asRange: NumericRange[BigInt] = from until to


  override def toString: String = size match
    case 0 => "Band(empty)"
    case 1 => s"Band(single: $from)"
    case _ => s"Band($from to ${to - 1})"

  private def min(x: BigInt, y: BigInt): BigInt = Ordering.BigInt.min(x, y)
  private def max(x: BigInt, y: BigInt): BigInt = Ordering.BigInt.max(x, y)
}

object Band {
  def empty: Band = Band(-1,-1)
  def around(value: BigInt, radius: BigInt): Band = Band(value - radius, value + radius + 1)
  def unionAll(bs: Iterable[Band]): List[Band] =
    val bands = bs.filterNot(_.isEmpty).toList.sortBy(_.from)
    if bands.isEmpty then List.empty
    else if bands.size == 1 then List(bands.head)
    else bands.tail.foldLeft(List(bands.head)) { (prevs, next) =>
      (prevs.head union next).map(_ :: prevs.tail).getOrElse(prevs prepended next)
    }.reverse

}

case class Sensor(pos: Point2D[BigInt], reachRadius: BigInt) {
  def bandOnRow(row: BigInt): Band =
    val verticalDifference = (pos.y - row).abs
    if verticalDifference > reachRadius then Band.empty
    else Band.around(pos.x, reachRadius - verticalDifference)

  def bandOnColumn(col: BigInt): Band =
    val horizontalDifference = (pos.x - col).abs
    if horizontalDifference > reachRadius then Band.empty
    else Band.around(pos.y, reachRadius - horizontalDifference)
}

object Day15 {
  opaque type Beacon = Point2D[BigInt]
  extension (b: Beacon)
    def position: Point2D[BigInt] = b

  def parseLine(line: String): (Sensor, Beacon) = line match
    case inputPattern(sx, sy, bx, by) =>
      val sensorPos = Point2D(BigInt(sx), BigInt(sy))
      val beaconPos = Point2D(BigInt(bx), BigInt(by))
      (Sensor(sensorPos, sensorPos manhattanDistance beaconPos), beaconPos)
    case _ => sys.error(s"Couldn't parse line: '$line'")

  def parse(input: String): (Set[Sensor], Set[Beacon]) =
    val values = input.split("\n").map(parseLine)
    (values.map(_._1).toSet, values.map(_._2).toSet)

  private val number: String = """(-?\d+)"""
  val inputPattern: Regex = s"""Sensor at x=$number, y=$number: closest beacon is at x=$number, y=$number""".r
}

def input = Day15.parse(Resource.getAsString("inputA.txt"))
def readPointsOnRow(row: BigInt, sensors: Iterable[Sensor], beacons: Iterable[Day15.Beacon]): BigInt =
  val readPoints = Band.unionAll(sensors.map(_.bandOnRow(row))).map(_.size).sum
  val beaconCount = beacons.count(_.position.y == row)
  readPoints - beaconCount

@main def part1 =
  val (sensors, beacons) = input
  val read = readPointsOnRow(2000000, sensors, beacons)
  println(s"There are $read left on row 2000000 which cannot contain a beacon")

def bandsOver(func: Sensor => BigInt => Band, i: BigInt)(sensors: Iterable[Sensor], searchBand: Band): List[Band] =
  Band.unionAll(sensors.map(func).map(_.apply(i)).map(_ intersect searchBand))

def searchNotCovered(sensors: Iterable[Sensor], searchBand: Band): Option[Point2D[BigInt]] =
  val row = searchBand.asRange.find(r => bandsOver(_.bandOnRow, r)(sensors, searchBand).size > 1)
  val col = searchBand.asRange.find(c => bandsOver(_.bandOnColumn, c)(sensors, searchBand).size > 1)
  for (r <- row; c <- col) yield Point2D(c, r)

def tuningFrequence(p: Point2D[BigInt]): BigInt = 4000000 * p.x + p.y

@main def part2 =
  val searchBand: Band = Band(0, 4000001)
  val point = searchNotCovered(input._1, searchBand)
  println(s"Found point at: $point with frequency: ${point.map(tuningFrequence)}")
