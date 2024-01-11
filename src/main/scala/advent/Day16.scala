package advent

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.io.Source

object Day16 {

  val testInput = """.|...\....
                    ||.-.\.....
                    |.....|-...
                    |........|.
                    |..........
                    |.........\
                    |..../.\\..
                    |.-.-/..|..
                    |.|....-|.\
                    |..//.|....""".stripMargin.split("\n").toList

  val prodInput = Source.fromResource("day16_1.txt").getLines.toList

  val (west, south, east, north) = (1, 2, 3, 4)

  def allSources(map: List[String]): Set[((Int, Int), Int)] = {
    map.indices.toList.flatMap(y => Set(((0, y), west), ((map.head.length - 1, y), east))).toSet ++
      (0 until map.head.length).toList.flatMap(x => Set(((x, 0), north), ((x, map.length - 1), south))).toSet
  }

  private def nextIdx(idx: (Int, Int), sourceDirection: Int): (Int, Int) = {
    if (sourceDirection == west) (idx._1 + 1, idx._2)
    else if (sourceDirection == south) (idx._1, idx._2 - 1)
    else if (sourceDirection == east) (idx._1 - 1, idx._2)
    else (idx._1, idx._2 + 1)
  }
  def light(source: ((Int, Int), Int), map: List[String]): Set[((Int, Int), Int)] = {
    @tailrec
    def innerLight(sources: Set[((Int, Int), Int)], passed: Set[((Int, Int), Int)]): Set[((Int, Int), Int)] = {
      val nextSources = sources.flatMap {
        case t@((x, y), sourceDirection) =>
          if (passed.contains(t)) Set.empty
          else {
            val c = map(y)(x)
            val continueSourceDirections = if (c == '|' && List(east, west).contains(sourceDirection)) Set(south, north)
            else if (c == '-' && List(north, south).contains(sourceDirection)) Set(east, west)
            else if (c == '/' && sourceDirection == west) Set(south)
            else if (c == '/' && sourceDirection == east) Set(north)
            else if (c == '/' && sourceDirection == north) Set(east)
            else if (c == '/' && sourceDirection == south) Set(west)
            else if (c == '\\' && sourceDirection == west) Set(north)
            else if (c == '\\' && sourceDirection == east) Set(south)
            else if (c == '\\' && sourceDirection == north) Set(west)
            else if (c == '\\' && sourceDirection == south) Set(east)
            else Set(sourceDirection)
            continueSourceDirections.map(nextDir => (nextIdx((x, y), nextDir), nextDir))
            }
          }.filter{ case ((x, y), _) => map.lift(y).flatMap(_.lift(x)).nonEmpty }
      if (nextSources.isEmpty) passed ++ sources
      else innerLight(nextSources, passed ++ sources)
    }
    innerLight(Set(source), Set.empty)
  }
}
