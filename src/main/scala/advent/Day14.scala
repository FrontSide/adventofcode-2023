package advent

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}
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

  def tiltRight(inp: List[String]): List[String] = inp.map{
      line =>
          line.reverse.split("#").map { // [.] [.O.] [O...]
            section =>
                  section.replace(".", "").padTo(section.length, ".").mkString
          }.mkString("#").padTo(line.length, "#").mkString.reverse
  }

  def load(inp: List[String]): Int = {
    inp.reverse.zipWithIndex.map{
      case (line, idx) =>
        line.count(_ == 'O') * (idx + 1)
    }.sum
  }

  def cycle(inpOrig: List[String], totalTimes: Long): List[String] = {

    @tailrec
    def innerCycle(inp: List[String], times: Long, mem: HashMap[String, (List[String], Long)] = HashMap.empty[String, (List[String], Long)]): List[String] = {
      if (times == 0) inp
      else {
        val inpS = inp.mkString
        val currentCycle = totalTimes - times
        val (resAfterCycle, cyclesLeft, nextMem) = mem.get(inpS) match {
          case Some((res, foundAt)) =>
            val repeatLength = currentCycle - foundAt
            val rem = ((totalTimes - foundAt) % repeatLength) - 1
            (res, rem, HashMap.empty[String, (List[String], Long)])
          case None =>
            val res = (1 to 4).foldLeft(inp) {
              (map, _) => tiltRight(rotateRight(map))
            }
            (res, times - 1, mem + (inpS -> (res, currentCycle)))
        }
        innerCycle(resAfterCycle, cyclesLeft, nextMem)
      }
    }

    innerCycle(inpOrig, totalTimes)

  }


}
