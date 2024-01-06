package advent

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day13Spec extends AnyFlatSpec with should.Matchers {

  "alternatives" should "generate all alternatives" in {
    Day13.alternatives(List("..#", "##.")).toList should be(List.empty)
  }

}
