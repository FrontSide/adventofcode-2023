package advent

import org.scalatest._
import flatspec._
import matchers._

class Day12Spec extends AnyFlatSpec with should.Matchers {

  "contPossibilities" should "correctly iterate through row" in {
    Day12.countPossibilities(Day12.Row("???",List(2, 1))) should be(0)
  }

  "substring" should "substring" in {
    "abcdef".charAt(5) should be('a')
  }

}
