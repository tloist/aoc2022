import munit.FunSuite

class Day06Tests extends FunSuite {

    test("Start of packet signal index is detected in example 1") {
        assertEquals(indexOfMarkerEnd("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), Some(7))
    }
  
    test("Start of packet signal index is detected in example 2") {
        assertEquals(indexOfMarkerEnd("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), Some(5))
    }

    test("Start of packet signal index is detected in example 3") {
        assertEquals(indexOfMarkerEnd("nppdvjthqldpwncqszvftbrmjlhg", 4), Some(6))
    }

    test("Start of packet signal index is detected in example 4") {
        assertEquals(indexOfMarkerEnd("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), Some(10))
    }

    test("Start of packet signal index is detected in example 5") {
        assertEquals(indexOfMarkerEnd("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), Some(11))
    }

    test("Start of message signal index is detected in example 1") {
        assertEquals(indexOfMarkerEnd("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), Some(19))
    }
  
    test("Start of message signal index is detected in example 2") {
        assertEquals(indexOfMarkerEnd("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), Some(23))
    }

    test("Start of message signal index is detected in example 3") {
        assertEquals(indexOfMarkerEnd("nppdvjthqldpwncqszvftbrmjlhg", 14), Some(23))
    }

    test("Start of message signal index is detected in example 4") {
        assertEquals(indexOfMarkerEnd("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), Some(29))
    }

    test("Start of message signal index is detected in example 5") {
        assertEquals(indexOfMarkerEnd("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), Some(26))
    }

}
