package advent


import scala.io.Source
import scala.math.pow


object Day4 {

  case class Card(id: Int, winningNumbers: List[Int], myNumbers: List[Int]) {
    val countOfMatches: Int = winningNumbers.intersect(myNumbers).length
  }

  case class SimpleCard(id: Int, matches: Int)

  val testInput = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  def findAllNumbers(str: String): List[Int] = {
    "\\d+".r.findAllIn (str).toList.map (_.toInt)
  }

  def getNumbers(card: String): Card = {
    val parsedCard = card.split(":").toList match {
      case List(titleStr, numbersStr) =>
        (findAllNumbers(titleStr).head,
        numbersStr.split("[|]").toList match {
          case List(winningNrsStr, myNrsStr) =>
            (findAllNumbers(winningNrsStr),
            findAllNumbers(myNrsStr))
        })
    }
    Card(parsedCard._1, parsedCard._2._1, parsedCard._2._2)
  }

  def getCard(cardNr: Int, cards: List[SimpleCard]): List[SimpleCard] = cards.find(_.id == cardNr).toList

  def getWinningCards(cardNr: Int, cards: List[SimpleCard]): List[SimpleCard] = {
    getCard(cardNr, cards) ++ getCard(cardNr, cards).map(_.matches).flatMap{
      case matchesCount if matchesCount >= 1 =>
        (cardNr + 1 to (cardNr + matchesCount)).toList.flatMap{
          nr => getWinningCards(nr, cards)
        }
      case _ => List.empty
    }
  }

  def run1 = {
    testInput.split("\n").map(getNumbers).toList.map(_.countOfMatches).map {
      case n: Int if n > 1 => pow(2, n - 1).toInt
      case n => n
    }.sum
  }

  def run2 = {
    //testInput.split("\n")
    val parsedCards = Source.fromResource("day4_1.txt").getLines.map(getNumbers).toList.map(card => SimpleCard(card.id, card.countOfMatches))
    parsedCards.flatMap(card => getWinningCards(card.id, parsedCards)).length
  }

}
