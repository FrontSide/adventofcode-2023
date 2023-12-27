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

    lazy val simpleExpanded: Universe = Universe(raw.flatMap {
        line =>
          if (line.contains('#')) List(line)
          else List(line, line)
      }.transpose.flatMap {
        line =>
          if (line.contains('#')) List(line)
          else List(line, line)
      }.transpose.map(_.mkString))

    lazy val emptyRowIdxs = {
      raw.zipWithIndex.collect{
        case (line, y) if !line.contains('#') => y
      }
    }

    lazy val emptyColumnIdxs = {
      raw.transpose.zipWithIndex.collect {
        case (line, x) if !line.contains('#') => x
      }
    }

    def smartGalaxies(multiplier: Int): List[(Long, Long)] = simpleGalaxies.map{
        case (x, y) => (x.toLong + emptyColumnIdxs.count(_ < x) * multiplier, y.toLong + emptyRowIdxs.count(_ < y) * multiplier)
      }

    lazy val simpleGalaxies: List[(Int, Int)] = raw.zipWithIndex.flatMap{
      case (line, y) =>
        line.zipWithIndex.collect{
          case (char, x) if (char == '#') => List((x, y))
        }.flatten
    }


    def distances(galaxies: List[(Long, Long)]) = {
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

    val universe = Universe(prod)
    println(universe)
    universe.distances(universe.smartGalaxies(999999)).sum / 2

  }

}
