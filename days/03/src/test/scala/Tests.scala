import munit.FunSuite

class Tests extends FunSuite {
  
    test("Splitting Rucksack into compartments: Example 1") {
        // … unchecked because List[String] in general isn't pattern matchable to exactly 2 elements: but here we know
        val first :: second :: Nil = Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp").compartments(2) : @unchecked
        assertEquals(first, "vJrwpWtwJgWr")
        assertEquals(second, "hcsFMMfFFhFp")
    }

    test("Splitting Rucksack into compartments: Example 2") {
        val first :: second :: Nil = Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").compartments(2) : @unchecked
        assertEquals(first, "jqHRNqRjqzjGDLGL")
        assertEquals(second, "rsFMfFZSrLrFZsSL")
    }

    test("Splitting Rucksack into compartments: Example 3") {
        val first :: second :: Nil = Rucksack("PmmdzqPrVvPwwTWBwg").compartments(2) : @unchecked
        assertEquals(first, "PmmdzqPrV")
        assertEquals(second, "vPwwTWBwg")
    }

    test("Identifying the common part in both compartments: Example 1") {
        assertEquals(Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp").commonInAllCompartments(2), Set('p'))
    }

    test("Identifying the common part in both compartments: Example 2") {
        assertEquals(Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").commonInAllCompartments(2), Set('L'))
    }

    test("Identifying the common part in both compartments: Example 2") {
        assertEquals(Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").commonInAllCompartments(2), Set('L'))
    }

    test("Identifying the common part in both compartments: Example 3") {
        assertEquals(Rucksack("PmmdzqPrVvPwwTWBwg").commonInAllCompartments(2), Set('P'))
    }

    test("Identifying the common part in both compartments: Example 4") {
        assertEquals(Rucksack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn").commonInAllCompartments(2), Set('v'))
    }

    test("Identifying the common part in both compartments: Example 5") {
        assertEquals(Rucksack("ttgJtRGJQctTZtZT").commonInAllCompartments(2), Set('t'))
    }

    test("Identifying the common part in both compartments: Example 6") {
        assertEquals(Rucksack("CrZsJsPPZsGzwwsLwLmpwMDw").commonInAllCompartments(2), Set('s'))
    }

    test("Prioritizing common items") {
        val rucksacks = readRucksacks("""|vJrwpWtwJgWrhcsFMMfFFhFp
                                         |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                                         |PmmdzqPrVvPwwTWBwg
                                         |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                                         |ttgJtRGJQctTZtZT
                                         |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin)
        val priorities = rucksacks
            .map(_.commonInAllCompartments(2))
            .map(_.head.priority)

        assertEquals(priorities, List(16, 38, 42, 22, 20, 19))
        assertEquals(priorities.sum, 157)
    }

    test("Identifying a groups common item type: Example 1") {
        val group1 = """|vJrwpWtwJgWrhcsFMMfFFhFp
                        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                        |PmmdzqPrVvPwwTWBwg""".stripMargin
        assertEquals(readRucksacks(group1).commonItemBadge, 'r')
    }

    test("Identifying a groups common item type: Example 2") {
        val group1 = """|wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                        |ttgJtRGJQctTZtZT
                        |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin
        assertEquals(readRucksacks(group1).commonItemBadge, 'Z')

    }

}
