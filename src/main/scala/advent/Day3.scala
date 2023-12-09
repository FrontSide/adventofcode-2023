package advent

import scala.io.Source

sealed trait Element

case class Number(value: Int, row: Int, column: Int) extends Element

case class Symbol(value: String, row: Int, column: Int) extends Element {
  def isRightNextTo(number: Number): Boolean = number.row == row && (number.column + number.value.toString.length) == column
  def isLeftNextTo(number: Number): Boolean = number.row == row && (number.column - 1) == column
  def isAbove(number: Number): Boolean = (number.row - 1) == row && column >= number.column - 1 && column <= (number.column + number.value.toString.length)
  def isBelow(number: Number): Boolean = (number.row + 1) == row && column >= number.column - 1 && column <= (number.column + number.value.toString.length)
  def isAdjacent(number: Number) = isRightNextTo(number) || isLeftNextTo(number) || isAbove(number) || isBelow(number)
}



object Day3 {

  val testSchematic = """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin.split("\n").toList

  private def getAdjacentSymbols(number: Number, symbols: List[Symbol]): List[Symbol] = {
    symbols.filter{
      symbol => symbol.isAdjacent(number)
    }
  }

  private def getAdjacentSymbols(numbers: List[Number], symbols: List[Symbol]): List[(Number, List[Symbol])] = {
    numbers.map(number => (number, getAdjacentSymbols(number, symbols)))
  }

  private def getAdjacentNumbers(symbol: Symbol, numbers: List[Number]): List[Number] = {
    numbers.filter {
      number => symbol.isAdjacent(number)
    }
  }

  private def getAdjacentNumbers(numbers: List[Number], symbols: List[Symbol]): List[(Symbol, List[Number])] = {
    symbols.map(symbol => (symbol, getAdjacentNumbers(symbol, numbers)))
  }

  private def getGears(numbers: List[Number], symbols: List[Symbol]): List[(Symbol, List[Number])] = {
    getAdjacentNumbers(numbers, symbols).filter{
      case (symbol: Symbol, numbers: List[Number]) => symbol.value == "*" && numbers.length == 2
    }
  }

  private def elements(input: List[String]): (List[Number], List[Symbol]) = {
    input.zipWithIndex.collect{
      case (line, row) =>
        "\\d+".r.findAllMatchIn(line).map(m => Number(m.group(0).toInt, row, m.start)).toList ++
          "[^\\d.]".r.findAllMatchIn(line).map(m => Symbol(m.group(0), row, m.start)).toList
    }.flatten.foldLeft(List.empty[Number], List.empty[Symbol]){
      case ((numbers, symbols), el) => el match {
        case el: Number => (el :: numbers, symbols)
        case el: Symbol => (numbers, el :: symbols)
      }
    }
  }

  private def numbersWithAdjacentSymbols(numbers: List[Number], symbols: List[Symbol]): List[Int] = {
    getAdjacentSymbols(numbers, symbols).filter(_._2.nonEmpty).map(_._1.value)
  }

  val run = {
    val (numbers, symbols) = elements(Source.fromResource("day3_1.txt").getLines.toList)
    //val (numbers, symbols) = elements(testSchematic)
    getGears(numbers, symbols).map(_._2.map(_.value)).map(_.product).sum
    //numbersWithAdjacentSymbols(numbers, symbols).sum
  }

}
