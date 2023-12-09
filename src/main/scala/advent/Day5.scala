package advent

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source

object Day5 {

  val testAlmanac =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4
      |""".stripMargin

  case class Mapping(destination: Long, source: Long, range: Long)

  private def convert(mappings: List[Mapping])(source: Long): Long = {
    mappings.find { mapping =>
      source >= mapping.source && source <= mapping.source + mapping.range
    } match {
      case Some(mapping) => source + (mapping.destination - mapping.source)
      case None => source
    }
  }

  private case class Almanac(seedRanges: List[NumericRange.Inclusive[Long]], allMappings: List[List[Mapping]])

  private def parseAlmanac(almanac: String) = {
    val groups = ":\n".r.split(almanac).toList
    val seedRanges = "\\d+".r.findAllIn(groups.head).toList.map(_.toLong).grouped(2).map {
      case List(rangeStart, rangeLength) => rangeStart to (rangeStart + rangeLength)
    }.toList

    val mappings = groups.tail.map(group =>
      group.split("\n").map{
        line =>
          "\\d+".r.findAllIn(line).toList.map(_.toLong) match {
            case List(a, b, c) => List(Mapping(a, b, c))
            case _ => List.empty
          }
      }.toList.flatten
    )
    Almanac(seedRanges, mappings)
  }

  @tailrec
  private def convertSeed(seed: Long)(allMappings: List[List[Mapping]]): Long = {
    allMappings match {
      case head :: tail => convertSeed(convert(head)(seed))(tail)
      case _ => seed
    }
  }

  def run = {
    //testAlmanac
    //Source.fromResource("day5_1.txt").mkString
    var min = Long.MaxValue
    val parsedAlmanac = parseAlmanac(Source.fromResource("day5_1.txt").mkString)
    parsedAlmanac.seedRanges.foreach{
      seedRange =>
        println(s"next seed range ${seedRange.length}")
        seedRange.foreach{
          seed =>
            if (seed % 10000000 == 0) println(s"seed ${seed}")
            val res = convertSeed(seed)(parsedAlmanac.allMappings)
            if (res < min) min = res
        }
    }
    min
  }


}
