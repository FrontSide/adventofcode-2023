package advent

import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  val testInput = """O....#....
                    |O.OO#....#
                    |.....##...
                    |OO.#O....O
                    |.O.....O#.
                    |O.#..O.#.#
                    |..O..#O..O
                    |.......O..
                    |#....###..
                    |#OO..#....""".stripMargin.split("\n").toList

  val prodInput = Source.fromResource("day14_1.txt").getLines.toList

  def rotateRight(inp: List[String]): List[String] = inp.transpose.map(_.reverse).map(_.mkString)

  def rotateLeft(inp: List[String]): List[String] = inp.transpose.reverse.map(_.mkString)

  def tiltRight(inp: List[String]): List[String] = {
    inp.map{
      line =>
        line.reverse.split("#").map{ // [.] [.O.] [O...]
          section =>
            section.replace(".", "").padTo(section.length, ".").mkString
        }.mkString("#").padTo(line.length, "#").mkString.reverse
    }
  }

  def load(inp: List[String]): Int = {
    inp.reverse.zipWithIndex.map{
      case (line, idx) =>
        line.count(_ == 'O') * (idx + 1)
    }.sum
  }

  @tailrec
  def cycle(inp: List[String], times: Int): List[String] = {
    if (times == 0) inp
    else {
      if (times % 100000 == 0) println(s"cycles left: $times")
      cycle((1 to 4).foldLeft(inp){
        (map, _) => tiltRight(rotateRight(map))
      }, times - 1)
    }
  }


}
