import Day15.Beacon
import munit.FunSuite
import better.files.Resource
import geo.Point2D

class Day15Test extends FunSuite {

  test("Finds 26 points on the 10 horizon") {
    val (sensors, beacons) = example
    val commonBands = Band.unionAll(sensors.map(_.bandOnRow(10)))
    assertEquals(commonBands.size, 1)
    assertEquals(commonBands.head.size, BigInt(27))

    val xs = commonBands.head.asRange.toSet
    val res = xs diff beacons.filter(_.position.y == 10).map(_.position.x)
    assertEquals(res.size, 26)
  }

  val searchBand: Band = Band(0, 21)

  test("Band can be unioned, if they are exactly next to each other") {
    val res = Band(0,12) union Band(12,24)
    assertEquals(res, Some(Band(0, 24)))
  }

  test("Part B") {
    val sensors = example._1

    val point = searchNotCovered(sensors, searchBand)
    assertEquals(point, Some(Point2D[BigInt](14, 11)))
    val value = point.map(tuningFrequence)
    assertEquals(value, Some(BigInt(56000011)))
  }

  def example: (Set[Sensor], Set[Beacon]) = Day15.parse(Resource.getAsString("example.txt"))
}
