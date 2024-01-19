package advent

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class DijkstraSpec extends AnyFlatSpec with should.Matchers {

  "dijks" should "find the shortest path" in {

    Dijkstra.TrackedDistances(Map.empty[(Int, Int),Set[(Int, (Int, Int))]]) ++ Set(Dijkstra.TrackedDistance((1, 2), 2, (0, 0)), Dijkstra.TrackedDistance((1, 2), 7, (6, 2)), Dijkstra.TrackedDistance((2, 2), 5, (6, 2))) should be(Dijkstra.TrackedDistances(Map((1, 2) -> Set((2, (0, 0)), (7, (6, 2))), (2, 2) -> Set((5, (6, 2))))))

    Dijkstra.shortestPath(0, 4, Dijkstra.edgesTestSet) should be(Left((List(0, 7, 6, 5, 4), 21)))
    Dijkstra.shortestPath(0, 8, Dijkstra.edgesTestSet) should be(Left((List(Dijkstra.TrackedDistance(4, 21, 5), Dijkstra.TrackedDistance(5, 11, 6), Dijkstra.TrackedDistance(6, 9, 7), Dijkstra.TrackedDistance(7, 8, 0), Dijkstra.TrackedDistance(0, 0, 0)), 21)))

  }


}
