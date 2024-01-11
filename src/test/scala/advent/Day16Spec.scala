package advent

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.collection.immutable.HashMap

class Day16Spec extends AnyFlatSpec with should.Matchers {

  "light" should "show all indices the light traversed" in {
    Day16.light(Day16.prodInput).map(_._1).size should be (46)
  }


}
