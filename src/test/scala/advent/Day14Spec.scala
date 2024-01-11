package advent

import advent.Day14.{prodInput, testInput}
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day14Spec extends AnyFlatSpec with should.Matchers {

  val testInputTitledNorth = """OOOO.#.O..
                               |OO..#....#
                               |OO..O##..O
                               |O..#.OO...
                               |........#.
                               |..#....#.#
                               |..O..#.O.O
                               |..O.......
                               |#....###..
                               |#....#....""".stripMargin

  val testFirstCycle = """.....#....
                         |....#...O#
                         |...OO##...
                         |.OO#......
                         |.....OOO#.
                         |.O#...O#.#
                         |....O#....
                         |......OOOO
                         |#...O###..
                         |#..OO#....""".stripMargin

  val testThreeCycle = """.....#....
                         |....#...O#
                         |.....##...
                         |..O#......
                         |.....OOO#.
                         |.O#...O#.#
                         |....O#...O
                         |.......OOO
                         |#...O###.O
                         |#.OOO#...O""".stripMargin

  "getLoad" should "calculate the load on the north bream" in {
    Day14.load(Day14.rotateLeft(Day14.tiltRight(Day14.rotateRight(testInput)))) should be(136)
    Day14.load(Day14.cycle(prodInput, 1000000000L)) should be(105008)
  }

  "cycle" should "cycle through all tilts" in {
    Day14.cycle(testInput, 1).mkString("\n") should be(testFirstCycle)
    Day14.cycle(testInput, 3).mkString("\n") should be(testThreeCycle)
  }

  "rotateRight" should "rotate map right" in {
    Day14.rotateRight(testInput).mkString("\n") should be(testFirstCycle)
  }

  "tiltRight" should "tilt map right" in {
    Day14.tiltRight(Day14.rotateRight(testInput)).mkString("\n") should be(testFirstCycle)
  }

}
