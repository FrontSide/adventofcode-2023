package advent

import javax.print.attribute.standard.Destination
import scala.collection.mutable
import scala.io.Source;

object Day17 {

    val testInput = """2413432311323
                      |3215453535623
                      |3255245654254
                      |3446585845452
                      |4546657867536
                      |1438598798454
                      |4457876987766
                      |3637877979653
                      |4654967986887
                      |4564679986453
                      |1224686865563
                      |2546548887735
                      |4322674655533
                      |""".stripMargin.split("\n").toList

  val prodInput = Source.fromResource("day17_1.txt").getLines.toList

  def getEdgeTo(inp: List[String], source: (Int, Int), destination: (Int, Int)) = inp.lift(destination._2).flatMap(_.lift(destination._1)).map{
    c =>Dijkstra.Edge(source, c.asDigit, destination)
  }

  def toEdges(inp: List[String]): Set[Dijkstra.Edge[(Int, Int)]] = {

    def neighbourEdges(x: Int, y: Int) = List((x + 1, y), (x, y + 1)).map {
      n => getEdgeTo(inp, (x, y), (n._1, n._2))
    }.collect {
      case Some(edge) => edge
    }

    inp.indices.flatMap(y => inp.head.indices.toList.flatMap(x => neighbourEdges(x, y))).toSet

  }

  val path = Dijkstra.shortestPath((0, 0), (12, 12), toEdges(testInput))

}
