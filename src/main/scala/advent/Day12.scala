package advent

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable
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


    lazy val expanded =
      Row(
        List(springs, springs, springs, springs, springs).mkString("?"),
        List(damageGroups, damageGroups, damageGroups, damageGroups, damageGroups).flatten)

    lazy val reg = {
      val groupReg = damageGroups
        .map { groupCount =>
          s"[#]{$groupCount}[.]"
        }
        .mkString("+")
      s"[^#]*$groupReg*$$".r
    }

  }

  def parseRows(raw: List[String]): List[Row] = {

    raw.flatMap {
      "(\\S+)\\s(\\S+)".r
        .findFirstMatchIn(_)
        .map { _match =>
          (_match.group(1), _match.group(2))
        }
        .collect {
          case (springs: String, damageGroups: String) =>
            println(s"springs ${springs} damageGroups ${damageGroups}")
            Row(springs, damageGroups.split(',').map(_.toInt).toList)
        }
        .toList
    }
  }

    //Dynamic programming / momoisation
    //store all already computed combinations for any given "Row" in a hash map
    var mm: mutable.Map[Row, Long] = mutable.Map.empty

    /*I solved the first part myself (code deleted) but as it wasn't fast enough to solve part 2 I used the solution from
    * https://www.youtube.com/watch?v=g3Ms5e7Jdqo */
    def countPossibilities(row: Row): Long = {
      mm.get(row) match {
        case Some(res) => res
        case None =>
          val res = (row.springs, row.damageGroups) match {
            case (s, gs) if s.isEmpty && gs.isEmpty => 1
            case (s, gs) if !s.contains("#") && gs.isEmpty => 1
            case (s, gs) if s.nonEmpty && gs.nonEmpty =>
              val resifdot: Option[Long] = Option.when(".?".contains(s.head))(countPossibilities(Row(s.tail, gs)))
              val resifqm: Option[Long] = Option.when("#?".contains(s.head)) {
                Option.when(gs.head <= s.length && !s.substring(0, gs.head).contains('.') && (gs.head == s.length || s.charAt(gs.head) != '#')) {
                  if (s.length < gs.head + 1) countPossibilities(Row(s.substring(gs.head), gs.tail))
                  else countPossibilities(Row(s.substring(gs.head + 1), gs.tail))
                }
              }.flatten
              resifdot.getOrElse(0L) + resifqm.getOrElse(0L)
            case _ => 0
          }
          mm(row) = res
          res
      }
    }

  lazy val run = parseRows(prodInput).map(_.expanded).map(countPossibilities).sum

}
