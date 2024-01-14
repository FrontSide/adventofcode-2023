package advent

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

  val (left, top) = (1, 2)

  case class Node(weightTo: Int, idx: (Int, Int)) {
    val x = idx._2
    val y = idx._1
    def neighbours(inp: List[String]): Set[(Node, Int)] = Set(((x + 1, y), left), ((x, y + 1), top)).map {
        case ((x, y), sourceDir) => (x, y, inp.lift(y).flatMap(_.lift(x)), sourceDir)
      }.collect {
        case (x, y, Some(c), sourceDir) => (Node(c.asDigit, (x, y)), sourceDir)
      }
  }

  def fmtWithPath(inp: List[String], visitedNodes: List[Node]) = inp.zipWithIndex.map{
      case (line, y) => line.zipWithIndex.map{
        case (c, x) => if (visitedNodes.map(_.idx).contains((x, y))) "X"
        else c
      }.mkString
    }.mkString("\n")

  def dijks(inp: List[String]): List[Node] = {

    val sptSet: mutable.ListBuffer[Node] = mutable.ListBuffer.empty
    def run(node: Node, sourceDirection: Int, straigthSeqLength: Int): Unit = {
      if (straigthSeqLength > 4) ()
      else {
        sptSet.append(node)
        node.neighbours(inp).filter(neighbour => !sptSet.map(_.idx).contains(neighbour._1.idx)).map(origNeighbour => (origNeighbour._1.copy(weightTo = origNeighbour._1.weightTo + node.weightTo), origNeighbour._2)).toList.sortBy(_._1.weightTo).foreach{
          case (nextNode, nextSourceDir) =>
            val nextStraightSeqLen = if (sourceDirection == nextSourceDir) straigthSeqLength + 1 else 0
            run(nextNode, nextSourceDir, nextStraightSeqLen)
        }
      }
    }
    run(Node(0, (0, 0)), left, 1)
    sptSet.toList

  }

}
