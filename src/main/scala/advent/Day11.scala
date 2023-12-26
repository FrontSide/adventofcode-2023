package advent

import scala.io.Source

object Day11 {

  val example1 = """...#......
                   |.......#..
                   |#.........
                   |..........
                   |......#...
                   |.#........
                   |.........#
                   |..........
                   |.......#..
                   |#...#.....""".stripMargin.split("\n").toList

  val prod = Source.fromResource("day11_1.txt").getLines.toList
  case class Universe(raw: List[String]) {
    lazy val expanded: Universe = Universe(raw.flatMap {
        line =>
          if (line.contains('#')) List(line)
          else List(line, line)
      }.transpose.flatMap {
        line =>
          if (line.contains('#')) List(line)
          else List(line, line)
      }.transpose.map(_.mkString))

    lazy val galaxies: List[(Int, Int)] = raw.zipWithIndex.flatMap{
      case (line, y) =>
        line.zipWithIndex.collect{
          case (char, x) if (char == '#') => List((x, y))
        }.flatten
    }

    lazy val distances = {
      galaxies.flatMap{
        left => galaxies.map{
           right =>
             (left._1 - right._1).abs + (left._2 - right._2).abs
        }
      }
    }

    override lazy val toString = raw.mkString("\n")

  }

  val run = {

    val expandedUniverse = Universe(prod).expanded
    println(expandedUniverse)
    expandedUniverse.distances.sum / 2

  }

}
