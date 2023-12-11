package advent

object Day7 {

    val cards: List[Char] = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

    object HandTypes extends Enumeration {
        type HandType = Value
        val  HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value
    }

    case class Hand(values: List[Char], bid: Int) {

        def nOfAKind(n: Int, exclude: Option[Char] = None): Option[Char] = {
            val cardsWithExclusion = exclude match {
                case Some(exclusionChar) => cards.filterNot(_ == exclusionChar)
                case None => cards
            }
            cardsWithExclusion.find(card => values.count(value => value == card) == n)
        }
        def isNOfAKind(n: Int) = nOfAKind(n).isDefined
        def isFullHouse = isNOfAKind(3) && isNOfAKind(2)

        def isTwoPair = {
            val firstPairCharOpt = nOfAKind(2)
            println(s"firstPairChar $firstPairCharOpt in $values")
            firstPairCharOpt match {
                case o: Some[Char] => {
                    val secPair = nOfAKind(2, o)
                    println(s"secPairChar $secPair in $values")
                    secPair.isDefined
                }
                case _ => false
            }
        }

        val highestType = {
            if (isNOfAKind(5)) HandTypes.FiveOfAKind
            else if (isNOfAKind(4)) HandTypes.FourOfAKind
            else if (isFullHouse) HandTypes.FullHouse
            else if (isNOfAKind(3)) HandTypes.ThreeOfAKind
            else if (isTwoPair) HandTypes.TwoPair
            else if (isNOfAKind(2)) HandTypes.OnePair
            else HandTypes.HighCard
        }

    }

    val testHandRaw = """32T3K 765
                        |T55J5 684
                        |KK677 28
                        |KTJJT 220
                        |QQQJA 483""".stripMargin.split("\n").toList

    def parseHands(rawLines: List[String]) = {
        rawLines.map{
            line => 
                line.split(" ").toList match {
                    case List(cards, bid) => Hand(cards.toCharArray.toList, bid.toInt)
                }
            }
    }

    def rankedHands(hands: List[Hand]): List[Hand] = ???

    def run = parseHands(testHandRaw).map(_.highestType)

}