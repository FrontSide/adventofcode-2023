package advent

import scala.io.Source;

object Day13 {

    val testInput = """#.##..##.
                  |..#.##.#.
                  |##......#
                  |##......#
                  |..#.##.#.
                  |..##..##.
                  |#.#.##.#.
                  |
                  |#...##..#
                  |#....#..#
                  |..##..###
                  |#####.##.
                  |#####.##.
                  |..##..###
                  |#....#..#""".stripMargin.split("\n\n").toList.map(_.split("\n").toList)

    val prodInput = Source.fromResource("day13_1.txt").mkString.split("\n\n").toList.map(_.split("\n").toList)

    def horizontalMirrorLine(inp: List[String], mult: Int = 1) = {
        (1 until inp.length).find{
            idx =>
                val (left, right) = inp.splitAt(idx)
                left.reverse.zip(right).forall{
                    t => t._1 == t._2
                }
        }.map(_ * mult)
    }

    def verticalMirrorLine(inp: List[String], mult: Int = 1) = horizontalMirrorLine(inp.transpose.map(_.mkString), mult)

    def findMirrorLine(inp: List[String]) = horizontalMirrorLine(inp, 100).orElse(verticalMirrorLine(inp))

    def alternatives(inp: List[String]): LazyList[List[String]] = {
        val inpT = inp.transpose
        val lineLength = inp.head.length
        val asString = inpT.mkString
        (0 until asString.length).to(LazyList).map{ changeIdx =>
            asString.zipWithIndex.map{
                case (c, idx) =>
                    if (idx == changeIdx) {
                        if (c == '.') '#'
                        else '.'
                    } else c
            }.mkString.grouped(lineLength).toList.transpose.map(_.mkString)
        }
    }

    lazy val run = testInput.map(block => alternatives(block).map(findMirrorLine).collectFirst{
        case Some(nr) => nr
    }).map(_.getOrElse(0)).sum
}
