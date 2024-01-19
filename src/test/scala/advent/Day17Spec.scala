package advent

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class Day17Spec extends AnyFlatSpec with should.Matchers {

  "dijks" should "find the shortest path" in {

      Day17.path should be(List.empty)

  }

}
