package advent

import scala.io.Source

object Day12 {

  val testInput = """???.### 1,1,3
                       |.??..??...?##. 1,1,3
                       |?#?#?#?#?#?#?#? 1,3,1,6
                       |????.#...#... 4,1,1
                       |????.######..#####. 1,6,5
                       |?###???????? 3,2,1""".stripMargin.split("\n").toList

  val prodInput = Source.fromResource("day12_1.txt").getLines.toList

  case class Row(springs: String, damageGroups: List[Int]) {

    val possibilities = 2

    val reg = {
      val groupReg = damageGroups
        .map { groupCount =>
          s"[#]{$groupCount}[.]"
        }
        .mkString("+")
      s"[^#]*$groupReg*$$".r
    }

    def wouldMatchGroups(possibleSprings: String): Boolean =
      reg.matches(possibleSprings)

    def withReplacedUnknown(replacement: String): String = {
      def replace(orig: String, repl: String): String = {
        if (orig.isEmpty) ""
        else {
          if (orig.head == '?') repl.head +: replace(orig.tail, repl.tail)
          else orig.head +: replace(orig.tail, repl)
        }
      }
      replace(springs, replacement)
    }

    val permutationsCount =
      (0 to math.pow(2, springs.count(_ == '?')).toInt - 1)
        .map { n =>
          val binString = n.toBinaryString
          val padding = "0".repeat(springs.count(_ == '?') - binString.length)
          s"$padding$binString".replace('0', '.').replace('1', '#')
        }
        .map(withReplacedUnknown)
        .toList
        .count(wouldMatchGroups)
  }

  def parseRows(raw: List[String]): List[Row] = {

    raw.flatMap {
      "(\\S+)\\s(\\S+)".r
        .findFirstMatchIn(_)
        .map { _match =>
          (_match.group(1), _match.group(2))
        }
        .collect { x =>
          x match {
            case (springs: String, damageGroups: String) =>
              println(s"springs ${springs} damageGroups ${damageGroups}")
              Row(springs, damageGroups.split(',').map(_.toInt).toList)
          }
        }
        .toList
    }

  }

  val run = parseRows(prodInput).map(_.permutationsCount).sum

}
