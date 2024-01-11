package advent

import advent.Day14.{prodInput, testInput}
import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.collection.immutable.HashMap

class Day15Spec extends AnyFlatSpec with should.Matchers {

  "holidayHash" should "calculate the correct hash" in {
    Day15.holidayHash("HASH") should be(52)
    Day15.holidayHash("rn") should be(0)
    Day15.holidayHash("cm") should be(0)
    Day15.holidayHash("qp") should be(1)
    Day15.holidayHash("pc") should be(3)
    Day15.testInput.map(Day15.holidayHash).sum should be(1320)
    Day15.prodInput.map(Day15.holidayHash).sum should be(517965)
  }

  "parsedStep" should "retrieve the step's label and focal Length" in {
    Day15.parsedStep("rn=1") should be((0, "rn", Some(1)))
    Day15.parsedStep("cm-") should be((0, "cm", None))
    Day15.parsedStep("qp=3") should be((1, "qp", Some(3)))
    Day15.parsedStep("pc=4") should be((3, "pc", Some(4)))
  }

  "operate" should "correctly replace lenses" in {
    Day15.operate((0, "cm", None), HashMap(0 -> List(("cm", 15)))) should be(HashMap(0 -> List.empty))
    Day15.operate((0, "cm", Some(17)), HashMap(0 -> List(("cm", 15)))) should be(HashMap(0 -> List(("cm", 17))))
    Day15.operate((0, "ab", Some(17)), HashMap(0 -> List(("cm", 15)))) should be(HashMap(0 -> List(("cm", 15), ("ab", 17))))
    Day15.operate((1, "ab", Some(17)), HashMap(0 -> List(("cm", 15)))) should be(HashMap(0 -> List(("cm", 15)), 1 -> List(("ab", 17))))
  }

  "runSteps" should "created boxes as per description" in {
    Day15.runSteps(Day15.testInput.map(Day15.parsedStep)) should be(Map(0 -> List(("rn", 1), ("cm", 2)), 1 -> List(), 3 -> List(("ot", 7), ("ab", 5), ("pc", 6))))
  }

  "focusingPower" should "compute the full focusing power of the lenses" in {
    Day15.focusingPower(Day15.runSteps(Day15.testInput.map(Day15.parsedStep))) should be(145)
    Day15.focusingPower(Day15.runSteps(Day15.prodInput.map(Day15.parsedStep))) should be(145)
  }

}
