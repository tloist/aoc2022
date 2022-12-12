import munit.FunSuite
import better.files.Resource

class HillHeightMapTest extends FunSuite {
  def exampleMap: HillHeightMap = HillHeightMap.read(Resource.getAsString("example.txt"))

  test("Reading a height map works") {
    val expected =
      """|Sabqponm
         |abcryxxl
         |accszExk
         |acctuvwj
         |abdefghi""".stripMargin
    assertEquals(exampleMap.asString(identity), expected)
  }

  test("Can we pass a huge drop?") {
    val input =
      """|Sabcdefghijka
         |zzzzzzzzzzzzb
         |onmlkjihgfedc
         |pqrstuvwxyzEa""".stripMargin
    val map = HillHeightMap.read(input)
    assertEquals(map.searchPath.map(_.size), Right(38))
  }

//  test("Finds path to exit: A*") {
//    exampleMap.searchPath.foreach(println)
//    assertEquals(exampleMap.searchPath.map(_.size), Right(31))
//  }

  test("Finds path to exit: BFS") {
    exampleMap.findExit().foreach(println)
    assertEquals(exampleMap.findExit().size, 31)

  }

}
