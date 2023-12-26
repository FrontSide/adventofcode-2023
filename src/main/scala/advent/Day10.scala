package advent

import advent.Day10.checkedNodesSimple

import scala.collection.immutable.{List, Queue}
import scala.collection.mutable
import scala.io.Source

object Day10 {

  private val simpleTestMap1 = """.....
                                 |.S-7.
                                 |.|.|.
                                 |.L-J.
                                 |.....""".stripMargin.split("\n").toList

  private val testMap1 = """-L|F7
                           |7S-7|
                           |L|7||
                           |-L-J|
                           |L|-JF""".stripMargin.split("\n").toList

  private val testMap2 = """..F7.
                           |.FJ|.
                           |SJ.L7
                           ||F--J
                           |LJ...""".stripMargin.split("\n").toList

  private val loopMap1 = """..........
                           |.S------7.
                           |.|F----7|.
                           |.||....||.
                           |.||....||.
                           |.|L-7F-J|.
                           |.|..||..|.
                           |.L--JL--J.
                           |..........""".stripMargin.split("\n").toList

  private val loopMap2 = """FF7FSF7F7F7F7F7F---7
                           |L|LJ||||||||||||F--J
                           |FL-7LJLJ||||||LJL-77
                           |F--JF--7||LJLJ7F7FJ-
                           |L---JF-JLJ.||-FJLJJ7
                           ||F|F-JF---7F7-L7L|7|
                           ||FFJF7L7F-JF7|JL---7
                           |7-L-JL7||F7|L7F-7F7|
                           |L.L7LFJ|||||FJL7||LJ
                           |L7JLJL-JLJLJL--JLJ.L""".stripMargin.split("\n").toList

  private val prodMap = Source.fromResource("day10_1.txt").getLines.toList

 case class PipeMap(raw: List[String]) {

   private def char(location: MapLocation): Option[Char] =
     raw.lift(location.y).flatMap(_.lift(location.x))

   def validatedConnectedLocations(location: MapLocation): List[MapLocation] = {
     char(location) match {
       case Some(character) if character == 'S' =>
         location.connectedLocations(character).filter {
           possibleNeighbour =>
             validatedConnectedLocations(possibleNeighbour).contains(location)
         }
       case other => other.toList.flatMap(location.connectedLocations)
     }
   }

   lazy val rootLocation: MapLocation = raw.zipWithIndex.flatMap {
     case (line, y) =>
       line.zipWithIndex.collect {
         case (char, x) if char == 'S' => MapLocation(x, y)
       }
   }.head

   //BFS Algorithm
   //The last node added to the "checkedNodes" queue is guaranteed to be the one farthest away
   //https://en.wikipedia.org/wiki/Breadth-first_search
   lazy val findFarthestNode: Unit = {
     while (toVisit.nonEmpty) {
       val ((x, y), dist) = toVisit.dequeue
       if (!checkedNodes.contains((x, y))) {
         checkedNodes.append(((x, y), dist))
         val neighbours = validatedConnectedLocations(MapLocation(x, y))
         toVisit.appendAll(neighbours.filter {
           neighbour =>
             !checkedNodes.map(checkedNode => (checkedNode._1._1, checkedNode._1._2)).contains((neighbour.x, neighbour.y))
         }.map(loc => ((loc.x, loc.y), dist + 1)))
       }
     }
   }

   lazy val expanded: PipeMap = {

     def top(belowChar: Char) = belowChar match {
       case 'L' | '|' | 'J' | 'S' => "X|X"
       case _ => "XXX"
     }

     def below(aboveChar: Char) = aboveChar match {
       case 'F' | '7' | '|' | 'S' => "X|X"
       case _ => "XXX"
     }

     def surround(char: Char) = char match {
       case 'L' => "XL-"
       case 'F' => "XF-"
       case '7' => "-7X"
       case '|' => "X|X"
       case '-' => "---"
       case 'J' => "-JX"
       case '.' => "X.X"
       case 'S' => "-S-"
     }

     PipeMap(raw.flatMap {
       line =>
         line.map {
           char =>
             List(top(char), surround(char), below(char))
         }.toList.foldLeft(List("", "", "")) {
           case (List(t1, m1, b1), List(t2, m2, b2)) => List(t1 + t2, m1 + m2, b1 + b2)
         }
     })
   }

   lazy val rawMutable = mutable.Seq.from(raw.map(line => mutable.Seq.from(line.toCharArray.toList)))

   lazy val flood: Unit = {
     while (toFlood.nonEmpty) {
       val (x, y) = toFlood.dequeue()
       if (char(MapLocation(x, y)).isEmpty || rawMutable(y)(x) == '0' || checkedNodesSimple(checkedNodes).contains((x, y))) ()
       else {
         rawMutable(y)(x) = '0'
         toFlood.appendAll(MapLocation(x, y).allSurroundingLocations.map(l => (l.x, l.y)))
       }
     }
   }

   override lazy val toString: String = raw.map(_.mkString).mkString("\n")

   lazy val indexed = raw.map(_.zipWithIndex.toList).zipWithIndex

   lazy val simpleMap: PipeMap = PipeMap(indexed.map {
     case (line, y) => line.map {
       case (_, x) =>
         if (checkedNodesSimple(checkedNodes).contains((x, y))) 'X'
         else '.'
     }.mkString
   })

   lazy val withExpandedRemoved: PipeMap = PipeMap(raw.grouped(3).map{
       threeLines =>
         threeLines(1).toList.grouped(3).map{
           threeCharacters =>
             threeCharacters(1)
         }.mkString
     }.toList)

 }

  case class MapLocation(x: Int, y: Int) {

    private lazy val left = MapLocation(x - 1, y)
    private lazy val right = MapLocation(x + 1, y)
    private lazy val top = MapLocation(x, y - 1)
    private lazy val bottom = MapLocation(x, y + 1)

    lazy val allSurroundingLocations: List[MapLocation] = List(top, left, bottom, right)

    def connectedLocations(char: Char): List[MapLocation] = {
      char match {
        case 'S' => allSurroundingLocations
        case 'L' => List(top, right)
        case 'F' => List(right, bottom)
        case '7' => List(bottom, left)
        case '|' => List(top, bottom)
        case '-' => List(right, left)
        case 'J' => List(top, left)
        case '.' | 'X' => List.empty
      }
    }

  }

  var toVisit: mutable.Queue[((Int, Int), Int)] = mutable.Queue.empty
  var checkedNodes: mutable.Queue[((Int, Int), Int)] = mutable.Queue.empty
  var toFlood: mutable.Queue[(Int, Int)] = mutable.Queue.empty


  def checkedNodesSimple(checkedNodes: mutable.Queue[((Int, Int), Int)]) =
    checkedNodes.map(_._1).toList

  //Solved with help from: https://www.youtube.com/watch?v=zhmzPQwgPg0
  //need to use Breadth-First Search (BFS) Algorithm
  //actually BFS is overkill because we don't have any junctions with more than
  //one input and one output (except for the S which has 0 inputs and 2 outputs)
  //so that means we are guaranteed to have a loop
  //and we could even just count the length of the loop and divide by two
  def run = {

    val map = PipeMap(prodMap)
    val expandedMap = map.expanded
    val rootLocation = expandedMap.rootLocation
    toVisit.append(((rootLocation.x, rootLocation.y), 0))
    expandedMap.findFarthestNode
    checkedNodesSimple(checkedNodes)

    /*

    toFlood.append((expandedMap.raw.head.length - 5, expandedMap.raw.length - 2))
    expandedMap.flood*/


    /*val fin = expandedMap.rawMutable.zipWithIndex.map{
      case (line, y) => line.zipWithIndex.map{
        case (c, x) =>
          if (checkedNodesSimple(checkedNodes).contains((x, y))) '0'
          else c
      }
    }*/

    /*For part two we are "expaning the map" and then "flood" it
    as per this comment on the video here: https://www.youtube.com/watch?v=r3i3XE9H4uw&lc=UgzxaOnoArso7thdlyN4AaABAg
    We could also have taken the "point-in-polygon" or ""ray casting" algorithm as used in the video itself
    */

    val simpleExpandedMap = expandedMap.simpleMap
    println(simpleExpandedMap)
    println("")

    toFlood.append((0, 0))
    simpleExpandedMap.flood
    val floodedMap = PipeMap(simpleExpandedMap.rawMutable.map(_.mkString).toList)
    println("flooded map")
    println(floodedMap)

    println("flooded map without expansion")
    val floodedMapReduces = floodedMap.withExpandedRemoved
    println(floodedMapReduces)

    floodedMapReduces.rawMutable.map(_.count(_ == '.')).sum

    /*
    val rootLocation = map.rootLocation
    map.validatedConnectedLocations(rootLocation)
    toVisit.append(((rootLocation.x, rootLocation.y), 0))
    map.findFarthestNode
    checkedNodes

    map.raw.zipWithIndex.map{
      case (line, y) =>
        line.zipWithIndex.map{
          case (char, x) =>
            if (checkedNodes.map(_._1).contains((x, y))) "X"
            else char
        }.mkString
    }.mkString("\n")

    //checkedNodes.map{case (coord, distance) => (distance, coord)}.max*/
  }
}
