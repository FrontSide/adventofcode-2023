package advent

import scala.io.Source

object Day9 {

  private val testInputRaw = """0 3 6 9 12 15
                       |1 3 6 10 15 21
                       |10 13 16 21 30 45""".stripMargin.split("\n").toList

  private val prodInputRaw = Source.fromResource("day9_1.txt").getLines.toList

  private def histories(rawInput: List[String]): List[List[Long]] =
    rawInput.map("(-?\\d+)".r.findAllIn(_).map(_.toLong).toList)

  private def getDiffs(ns: List[Long]): List[List[Long]] = {
    if (ns.forall(_ == 0)) List(ns :+ 0)
    else ns :: getDiffs(ns.zip(ns.drop(1)).map{case (a, b) => b - a})
  }

  private def extendB(a: List[Long], b: List[Long]) = b :+ b.last - a.last
  private def extrapolate(reversedTree: List[List[Long]], modifier: List[Long] => List[Long]): List[List[Long]] = reversedTree.map(modifier) match {
    case last :: secondLast :: tip if tip.isEmpty =>
      List(last, extendB(last, secondLast))
    case last :: secondLast :: tip =>
      last :: extrapolate(List(extendB(last, secondLast)) ++ tip, l => l)
  }

  private def treeToString(tree: List[List[Long]]): String = {
    tree.zipWithIndex.map{case (nrs, spaces) => {
      val sp = " " * spaces
      s"$sp${nrs.mkString(" ")}"
    }}.mkString("\n")
  }

  def run = {
    histories(prodInputRaw).map(history => extrapolate(getDiffs(history).reverse, l => l.reverse).map(_.last)).map(_.last).sum
  }

}
