package advent

import scala.annotation.tailrec
import scala.io.Source

object Day7 {

    val cards: Map[Char, Int] = List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A').zipWithIndex.toMap

    object HandTypes extends Enumeration {
        type HandType = Value
        val  HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value
    }

    case class Hand(values: List[Char], bid: Long) extends Ordered[Hand] {

        @tailrec
        private def compareAtIndex(other: Hand, idx: Int = 0): Int = {
            cards(values(idx)).compare(cards(other.values(idx))) match {
                case n if n == 0 => compareAtIndex(other, idx + 1)
                case n => n
            }
        }
        def compare(that: Hand): Int = {
            if (highestType > that.highestType) 1
            else if (highestType < that.highestType) -1
            else compareAtIndex(that)
        }

        private def nOfAKind(n: Int, exclude: List[Char] = List.empty, subJs: Boolean = true): Option[Char] = {
            val cwe = cards.toList.sortBy(_._2).map(_._1).reverse.filterNot(c => (exclude :+ 'J').contains(c))
            println(f"check for $n in $cwe")
            cwe.find{
                card => values.map {
                    case x: Char if x == 'J' && subJs => card
                    case x: Char => x
                }.count(value => value == card) == n
            }
        }
        private def isNOfAKind(n: Int) = nOfAKind(n).isDefined
        private def isFullHouse = {
            (for {
                threeChar <- nOfAKind(3)
                twoChar <- nOfAKind(2, List(threeChar), subJs = false)
            } yield {
                println(s"three: ${threeChar}, two: $twoChar")
                twoChar
            }).isDefined
        }

        private def isTwoPair = {
            val firstPairCharOpt = nOfAKind(2)
            println(s"firstPairChar $firstPairCharOpt in $values")
            firstPairCharOpt match {
                case Some(c) => {
                    val secPair = nOfAKind(2, List(c), subJs = false)
                    println(s"secPairChar $secPair in $values")
                    secPair.isDefined
                }
                case _ => false
            }
        }

        val highestType: HandTypes.HandType = {
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

    val prodHandRaw = Source.fromResource("day7_1.txt").getLines.toList

    private def parseHands(rawLines: List[String]): List[Hand] = {
        rawLines.map{
            line => 
                line.split(" ").toList match {
                    case List(cards, bid) => Hand(cards.toCharArray.toList, bid.toInt)
                }
            }
    }

    def run = {
        //Hand(List('J', '8', '8', '8', '8'), 774).highestType
        //'8' == '8'
        parseHands(prodHandRaw).sorted.zipWithIndex.map{
            case (hand, idx) => hand.bid * (idx + 1)
        }.sum
        /*println(prodHandRaw.length)*/
        /*parseHands(prodHandRaw).sorted.map{
            x =>
                println((x, x.highestType))
            x
        }*/
    }

}