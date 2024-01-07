package advent

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day13Spec extends AnyFlatSpec with should.Matchers {

  /*"alternatives" should "generate all alternatives" in {
    Day13.alternatives(List("..#", "##.")).toList should be(List.empty)
  }*/

  "run" should "run" in {
    Day13.run should be(List.empty)
  }

  "alternatives" should "run correctly" in {
    Day13.alternatives(Day13.testInput.last).length should be(
      1
    )
  }

}
