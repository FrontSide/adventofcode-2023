package advent

import com.sun.source.tree.Tree

import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  val test1InstructionsRaw = """RL
                               |
                               |AAA = (BBB, CCC)
                               |BBB = (DDD, EEE)
                               |CCC = (ZZZ, GGG)
                               |DDD = (DDD, DDD)
                               |EEE = (EEE, EEE)
                               |GGG = (GGG, GGG)
                               |ZZZ = (ZZZ, ZZZ)""".stripMargin.split("\n").toList

  val test2InstructionsRaw = """LLR
                               |
                               |AAA = (BBB, BBB)
                               |BBB = (AAA, ZZZ)
                               |ZZZ = (ZZZ, ZZZ)""".stripMargin.split("\n").toList

  val test3InstructionsRaw = """LR
                               |
                               |11A = (11B, XXX)
                               |11B = (XXX, 11Z)
                               |11Z = (11B, XXX)
                               |22A = (22B, XXX)
                               |22B = (22C, 22C)
                               |22C = (22Z, 22Z)
                               |22Z = (22B, 22B)
                               |XXX = (XXX, XXX)""".stripMargin.split("\n").toList

  val prodInstructionsRaw = Source.fromResource("day8_1.txt").getLines.toList
  def parseInstructions(rawInstructions: List[String]) = {
    (rawInstructions.head, rawInstructions.tail.flatMap{
      rawLine =>
        "(\\w{3}) = [(](\\w{3}), (\\w{3})[)]".r.findFirstMatchIn(rawLine).toList.map{
          m =>
            (m.group(1), (m.group(2), m.group(3)))
        }
    }.toMap)
  }

  private var paths: Map[String, (String, String)] = Map.empty

  @tailrec
  private def findPath(directions: String, paths: Map[String, (String, String)], roots: List[String], iteration: Long = 0): Long = {
    if (roots.forall(_.endsWith("Z"))) iteration
    else {
      if (iteration % 10000000== 0) println(s"$roots $iteration")
      val direction = directions((iteration % directions.length).toInt)
      if (direction == 'L') findPath(directions, paths, roots.map(paths(_)._1), iteration + 1)
      else findPath(directions, paths, roots.map(paths(_)._2), iteration + 1)
    }
  }

  @tailrec
  def firstFinal(directions: String, paths: Map[String, (String, String)], root: String, iteration: Long = 0): Long = {
    val directionIdx = (iteration % directions.length).toInt
    if (root.endsWith("Z")) iteration
    else if (directions(directionIdx) == 'L') firstFinal(directions, paths, paths(root)._1, iteration + 1)
    else firstFinal(directions, paths, paths(root)._2, iteration + 1)
  }

  @tailrec
  def gcd(a: Long, b: Long): Long = b match {
    case 0 => a
    case _ => gcd(b, a % b) // tail recursion
  }
  def lcm(ns: List[Long]): Long = {
    ns.foldLeft(1L){case (a, b) => a * b / gcd(a, b)}
  }

  //part 2 solved with help from: https://www.youtube.com/watch?v=2EJhqjSxhps
  def run = {
    val instructions = parseInstructions(prodInstructionsRaw)
    val roots = instructions._2.keys.toList.filter(_.endsWith("A"))
    lcm(roots.map(firstFinal(instructions._1, instructions._2, _)))
  }


}
