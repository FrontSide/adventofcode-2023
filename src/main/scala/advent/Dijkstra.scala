package advent

import javax.print.attribute.standard.Destination
import scala.annotation.tailrec
import scala.collection.mutable

object Dijkstra {

  case class Edge[T](source: T, distance: Int, destination: T)
  case class TrackedDistance[T](destination: T, totalDistance: Int, closestSource: T) {
    val mapElement: (T, (Int, T)) = destination -> (totalDistance, closestSource)
  }
  case class TrackedDistances[T](distances: Map[T, Set[(Int, T)]]) {
    def nextClosestNode(unvisited: Set[T]) =
      distances.filter(n => unvisited.contains(n._1)).map(n => (n._1, n._2.minBy(_._1))).minByOption(_._2._1).map(node => TrackedDistance(node._1, node._2._1, node._2._2))
    def getByDestination(destination: T) = distances.get(destination)

    def +(distance: TrackedDistance[T]) =
      TrackedDistances(distances + (distance.destination -> getByDestination(distance.destination).getOrElse(Set.empty).+((distance.totalDistance, distance.closestSource))))

    def ++(newDistances: Set[TrackedDistance[T]]): TrackedDistances[T] = TrackedDistances(newDistances.foldLeft(distances) {
      (updatedDistances, newDistanceToAdd) => (TrackedDistances(updatedDistances) + newDistanceToAdd).distances
    })

  }

  def toPath[T](orderedDistances: List[TrackedDistance[T]]) = orderedDistances.map(_.destination).reverse

  val edgesTestSet: Set[Edge[Int]] = Set(
    Edge(0, 4, 1), Edge(0, 8, 7), Edge(1, 11, 7), Edge(1, 8, 2), Edge(7, 7, 8), Edge(2, 2, 8),
    Edge(8, 6, 6), Edge(6, 2, 5), Edge(5, 10, 4), Edge(3, 9, 4), Edge(3, 14, 5), Edge(7, 1, 6),
    Edge(2, 4, 5), Edge(2, 7, 3)
  )

  private def edgesToNeighbours[T](sourceNode: T, edges: Set[Edge[T]]) = edges.filter(_.source == sourceNode)


  val (north, east, west, south) = (1, 2, 3, 4)

  def getDirection(source: (Int, Int), destination: (Int, Int)) = (destination._1 - source._1, destination._2 - source._2) match {
      case (1, 0) => east
      case (-1, 0) => west
      case (0, 1) => south
      case (0, -1) => north
    }

  def buildDistances[T](edges: Set[Edge[T]], startNode: T) = {

    @tailrec
    def run(startNode: TrackedDistance[T], trackedDistances: TrackedDistances[T], unvisitedNodes: Set[T]): TrackedDistances[T] = {

      val newTrackedDistances = (trackedDistances + startNode) ++ edgesToNeighbours(startNode.destination, edges).map {
        edgeToNeighbour =>
          TrackedDistance(edgeToNeighbour.destination, edgeToNeighbour.distance + startNode.totalDistance, startNode.destination)
      }

      newTrackedDistances.nextClosestNode(unvisitedNodes - startNode.destination) match {
        case None => trackedDistances
        case Some(next: TrackedDistance[T]) =>
          run(next, newTrackedDistances, unvisitedNodes - startNode.destination)
      }

    }

    run(TrackedDistance(startNode, 0, startNode), TrackedDistances(Map.empty), edges.map(_.source))

  }

  def shortestPath[T](start: T, end: T, edges: Set[Edge[T]]): Either[(List[T], Int), Exception] = {

    @tailrec
    def findPath(start: T, end: T, distances: TrackedDistances[T], path: List[TrackedDistance[T]]): Either[(List[T], Int), Exception] =
      distances.getByDestination(end) match {
        case None => Right(new RuntimeException(f"no entry for node $end"))
        case Some(sources) =>
          val (closestSourceDistance, closestSource) = sources.minBy(_._1)
          if (start != end) findPath(start, closestSource, distances, path :+ TrackedDistance(end, closestSourceDistance, closestSource))
          else {
            val finalPaths = path :+ TrackedDistance(end, closestSourceDistance, closestSource)
            Left(toPath(finalPaths), finalPaths.head.totalDistance - finalPaths.last.totalDistance)
          }
      }

    findPath(start, end, buildDistances(edges, start), List.empty)
  }

  def shortestPathWithStraightRestriction[T](start: T, end: T, edges: Set[Edge[T]], maxStraight: Int = 3): Either[Exception, List[(List[T], Int)]] = {

    @tailrec
    def findPath(start: T, end: T, distances: TrackedDistances[T], path: List[TrackedDistance[T]]): Either[Exception, List[(List[T], Int)]] =
      distances.getByDestination(end) match {
        case None => Left(new RuntimeException(f"no entry for node $end"))
        case Some(sources) =>
          val resSet = sources.map {
            case (dist, source) =>
              if (start != end) findPath(start, source, distances, path :+ TrackedDistance(end, dist, source))
              else {
                val finalPaths = path :+ TrackedDistance(end, dist, source)
                Right(toPath(finalPaths), finalPaths.head.totalDistance - finalPaths.last.totalDistance)
              }
          }
          val errors = resSet.filter(_.isLeft)
          if (errors.nonEmpty) errors.head
          else resSet.map(_.g)


      }

    findPath(start, end, buildDistances(edges, start), List.empty)
  }

}
