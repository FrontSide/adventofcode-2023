package advent

import scala.collection.Map
import scala.io.Source

case class Record(games: List[Game])
case class Game(id: Option[Integer], revelations: List[Revelation])
case class Revelation(sets: List[SquareSet]) {
  val toMap: Map[String, Int] = sets.map(set => (set.color, set.count.toInt)).toMap
}
case class SquareSet(count: Integer, color: String)

object Day2 {

  val testRecordRaw = List(
    "Game1: 3 blue, 4 red;1 red, 2 green, 6 blue;2 green",
    "Game2: 1 blue, 2 green;3 green, 4 blue, 1 red;1 green, 1 blue",
    "Game3: 8 green, 6 blue, 20 red;5 blue, 4 red, 13 green;5 green, 1 red",
    "Game4: 1 green, 3 red, 6 blue;3 green, 6 red;3 green, 15 blue, 14 red",
    "Game5: 6 red, 1 blue, 3 green;2 blue, 1 red, 2 green"
  )

  val testValidationRevelation = Revelation(List(SquareSet(12, "red"), SquareSet(13, "green"), SquareSet(14, "blue")))

  private def parseSquareSet(rawSquareSet: String): Option[SquareSet] = {
    "(\\d+)\\s+(\\w*)".r.findFirstMatchIn(rawSquareSet).map(m => SquareSet(m.group(1).toInt, m.group(2)))
  }

  private def parseRevelation(rawRevelation: String): Revelation = {
    Revelation(rawRevelation.split(",").map(parseSquareSet).toList.collect{
      case Some(ss: SquareSet) => ss
    })
  }

 private def parseRevelations(rawRevelations: String): List[Revelation] = {
   rawRevelations.split(";").map(parseRevelation).toList
  }

  private def parseGame(rawGame: String): Game = {
    rawGame.split(":") match {
      case Array(gameId, remainder) =>
        Game("\\d+".r.findFirstIn(gameId).map(_.toInt), parseRevelations(remainder))
    }
  }

  private def parseRecord(rawRecord: List[String]): Record = {
    Record(rawRecord.map(parseGame))
  }

  private def isPossibelRevelation(revelation: Revelation, validation: Revelation): Boolean = {
    validation.sets.forall{
      validationSet =>
        println(s"check color ${validationSet.color} of count ${validationSet.count} in set ${revelation.toMap}")
        val isPoss = validationSet.count >= revelation.toMap.getOrElse(validationSet.color, 0)
        println(isPoss)
        isPoss
    }
  }

  private def isPossibelGame(game: Game, validation: Revelation): Boolean = {
    game.revelations.forall(revelation => isPossibelRevelation(revelation, validation))
  }


  private def validate(record: Record, validation: Revelation): Integer = {
    record.games.filter{
      game =>
      println(s"Game ${game.id}")
      val isPoss = isPossibelGame(game, validation)
        println(s"Game is poss ${isPoss}")
        isPoss
    }.map(_.id).collect{
      case Some(id) => id.toInt
    }.sum
  }

private def minimumRevelationRequired(game: Game): Revelation = {
  Revelation(game.revelations.flatMap(_.sets).map(set => (set.color, set.count)).groupBy(_._1).collect{
    case (color, colorCounts) => SquareSet(colorCounts.map(_._2).max, color)
  }.toList)
}

  private def powerOfRevelation(revelation: Revelation) = {
    revelation.sets.map(_.count.toInt).product
  }

  //val run: Unit = print(validate(parseRecord(Source.fromResource("day2_1.txt").getLines.toList), testValidationRevelation))
  val run: Unit = print(parseRecord(Source.fromResource("day2_1.txt").getLines.toList).games.map(game => powerOfRevelation(minimumRevelationRequired(game))).sum)

}