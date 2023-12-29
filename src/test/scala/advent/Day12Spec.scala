package advent

import org.scalatest._
import flatspec._
import matchers._

class Day12Spec extends AnyFlatSpec with should.Matchers {

  "reg" should "generate correct group regex" in {
    Day12.Row("?###????????", List(2, 4, 3)).reg.toString should be(
      "[^#]*[#]{2}[.]+[#]{4}[.]+[#]{3}[.]*$"
    )
  }

  "wouldMatchGroups" should "correctly identify if the damage groups would match" in {
    val testRow = Day12.Row("?###????????", List(3, 2, 1))

    testRow.wouldMatchGroups(".###.##.#...") should be(true)
    testRow.wouldMatchGroups(".###..##...#") should be(true)
    testRow.wouldMatchGroups("###..##...#.") should be(true)
    testRow.wouldMatchGroups(".##..##.#...") should be(false)
    testRow.wouldMatchGroups("...........") should be(false)
    testRow.wouldMatchGroups("######") should be(false)
    testRow.wouldMatchGroups("#####..##.#.") should be(false)
  }

  /*"allPossibilities" should "correctly generate possible combinations" in {
    Day12.Row(".??.###.??..#", List(3, 2, 1)).allPossibilities should be(
      List(
        "....",
        "...#",
        "..#.",
        "..##",
        ".#..",
        ".#.#",
        ".##.",
        ".###",
        "#...",
        "#..#",
        "#.#.",
        "#.##",
        "##..",
        "##.#",
        "###.",
        "####"
      )
    )
  }*/

  "withReplacedUnknown" should "correctly replace all questionmarks" in {
    Day12
      .Row(".??.###.??..#", List(3, 2, 1))
      .withReplacedUnknown(".#.#") should be("..#.###..#..#")
  }

  "permutationsCount" should "correctly find all alternatives" in {
    Day12
      .Row("?###????????", List(3, 2, 1))
      .permutationsCount should be(10)
  }

  "testCase" should "return correct sum" in {
    Day12.parseRows(Day12.testInput).map(_.permutationsCount).sum should be(21)
  }

}
