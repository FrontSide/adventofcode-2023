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
                  |#....#..#""".stripMargin
    .split("\n\n")
    .toList
    .map(_.split("\n").toList)

  val prodInput = Source
    .fromResource("day13_1.txt")
    .mkString
    .split("\n\n")
    .toList
    .map(_.split("\n").toList)

  def horizontalMirrorLine(
      inp: List[String],
      mult: Int = 1,
      ignoreResult: Option[Int] = None
  ) = {
    (1 until inp.length)
      .find { idx =>
        val (left, right) = inp.splitAt(idx)
        val res = idx * mult

        left.reverse.zip(right).forall { t =>
          t._1 == t._2
        } && ignoreResult.map(_ != res).getOrElse(true)
      }
      .map(_ * mult)
  }

  def verticalMirrorLine(
      inp: List[String],
      mult: Int = 1,
      ignoreResult: Option[Int] = None
  ) =
    horizontalMirrorLine(inp.transpose.map(_.mkString), mult, ignoreResult)

  def findMirrorLine(inp: List[String], ignoreResult: Option[Int] = None) =
    horizontalMirrorLine(inp, 100, ignoreResult).orElse(
      verticalMirrorLine(inp, 1, ignoreResult)
    )

  def alternatives(inp: List[String]): LazyList[List[String]] = {
    val lineLength = inp.head.length
    val asString = inp.mkString
    (0 until asString.length).to(LazyList).map { changeIdx =>
      asString.zipWithIndex
        .map { case (c, idx) =>
          if (idx == changeIdx) {
            if (c == '.') '#'
            else '.'
          } else c
        }
        .mkString
        .grouped(lineLength)
        .toList
        .map(_.mkString)
    }
  }

  def prettyBlock(block: List[String]) = block.mkString("\n")

  def pretty(inp: List[List[String]]) =
    inp.map(prettyBlock).mkString("\n\n")

  lazy val run =
    prodInput
      .map { _map =>
        val originalReflectionLine = findMirrorLine(_map).getOrElse(-1)
        val alts = alternatives(_map)
        alts
          .map { alt =>
            findMirrorLine(alt, Some(originalReflectionLine))
          }
          .collectFirst { case Some(newVal) =>
            newVal
          }
      }
      .map(_.get)
      .sum

  lazy val runOrig = prodInput
    .map(findMirrorLine(_))
    .collect { case Some(nr) =>
      nr
    }
    .sum
}
