package advent

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class Day17Spec extends AnyFlatSpec with should.Matchers {

  "dijks" should "find the shortest path" in {
    Day17.dijks(Day17.prodInput).find(_.idx == (Day17.prodInput.head.length - 1, Day17.prodInput.length - 1)) should be(List.empty)

    val fut1 = Future{
      Thread.sleep(1000)
      2
    }

    val fut2 = Future{
      Thread.sleep(1000)
      12
    }

    val futs = Set(2, 10)
    val s = Future.traverse[Int, Int, Set](futs){
      nr => Future{
        Thread.sleep(200)
        nr + 30
      }
    }

    Await.result(s, 2.seconds)

  }


}
