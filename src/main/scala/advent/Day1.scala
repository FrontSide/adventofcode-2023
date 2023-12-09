package advent

import scala.io.Source
import scala.util.matching.Regex

object Day1 {

  private val mapper = Map[String, Integer](
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  val testSet = List(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  )

  private def resolveMatch(matcher: (Regex, String) => Option[String])(str: String): Option[Integer] = {
    val words = mapper.keys.toList.mkString("|")
    // Use a capturing group inside a lookahead.
    // The lookahead captures the text you're interested in,
    // but the actual match is technically the zero-width substring before the lookahead,
    // so the matches are technically non-overlapping:
    // https://stackoverflow.com/questions/5616822/how-to-use-regex-to-find-all-overlapping-matches
    val reg = s"(?=([1-9]|$words))".r
    println(s"$reg, $str")
    matcher(reg, str).map {
      firstOcc =>
        mapper.getOrElse(firstOcc, firstOcc.toInt)
    }
  }

  private def getFirst(str: String): Option[Integer] = {
    resolveMatch((reg, s) => reg.findFirstMatchIn(s).map(_.group(1)))(str)
  }

  private def getLast(str: String): Option[Integer] = {
    resolveMatch((reg, s)=> reg.findAllMatchIn(s).toList.lastOption.map(_.group(1)))(str)
  }

  def run(): Unit = {
    println {

      Source.fromResource("day1_1.txt").getLines.map {
      //testSet.map{
      str =>
        println(s"line is: $str")
          for {
            first <- getFirst(str)
            last <- getLast(str)
          } yield {
            println(s"$first$last of $str")
            s"$first$last".toInt
          }
      }.collect(_.getOrElse(0)).sum

    }
  }
}


