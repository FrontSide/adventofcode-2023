package advent

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.collection.immutable.HashMap

class Day16Spec extends AnyFlatSpec with should.Matchers {

  "light" should "show all indices the light traversed" in {
    Day16.allSources(Day16.prodInput).map( source => Day16.light(source, Day16.prodInput).map(_._1).size).max should be (46)
  }


}
