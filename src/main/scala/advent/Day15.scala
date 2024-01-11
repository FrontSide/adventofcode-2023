package advent

import scala.io.Source

object Day15 {

  val testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".split(",").toList
  val prodInput = Source.fromResource("day15_1.txt").getLines.toList.head.split(",").toList

  def holidayHash(inp: String): Int = inp.toCharArray.map(_.toInt).foldLeft(0){
      (acc, charCode) =>
        ((acc + charCode) * 17) % 256
    }

  def parsedStep(step: String): (Int, String, Option[Int]) = step.split("[=-]") match {
    case Array(label, focal) => (holidayHash(label), label, Some(focal.toInt))
    case Array(label) => (holidayHash(label), label, None)
  }

  def operate(parsedStep: (Int, String, Option[Int]), boxes: Map[Int, List[(String, Int)]]): Map[Int, List[(String, Int)]] = parsedStep match {
    case (boxNr, label, None) => boxes.get(boxNr) match {
      case None => boxes
      case Some(lenses) => boxes + (boxNr -> lenses.filterNot(_._1 == label))
    }
    case (boxNr, label, Some(focalLength)) => boxes.get(boxNr) match {
      case None => boxes + (boxNr -> List((label, focalLength)))
      case Some(lenses) =>
        if (!lenses.map(_._1).contains(label)) boxes + (boxNr -> (lenses :+ (label, focalLength)))
        else boxes + (boxNr -> lenses.map {
          case (existingLensLabel, _) if existingLensLabel == label => (label, focalLength)
          case o => o
        })
    }
  }

  def runSteps(parsedSteps: List[(Int, String, Option[Int])]): Map[Int, List[(String, Int)]] = parsedSteps.foldLeft(Map.empty[Int, List[(String, Int)]]){
      (boxes, step) => operate(step, boxes)
    }

  def focusingPower(boxes: Map[Int, List[(String, Int)]]): Int = boxes.flatMap{
      case (boxNr, lenses) =>
        lenses.zipWithIndex.map{
          case ((_, focalLength), lensIdx) => (1 + boxNr) * (lensIdx + 1) * focalLength
        }
    }.sum

}
