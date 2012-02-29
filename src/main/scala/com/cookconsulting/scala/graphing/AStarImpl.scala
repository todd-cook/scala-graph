package com.cookconsulting.scala.graphing

import java.util.PriorityQueue
import collection.mutable.{ListBuffer, HashMap}

/**
 * @author todd
 * @since 9/17/11 4:48 PM
 */

class AStarImpl (val graph: Graph) {

  /**
   * The set of tentative nodes to be evaluated, initially containing the start node
   */
  val openset = new PriorityQueue[Path]()
  val mindists = new HashMap[Node, Double]()

  val radianConversion = 180d / java.lang.Math.PI;

  //diagnostic methods
  def getNumberOfPaths() = openset.size()

  def getMinDistancesSize() = mindists.size

  def reconstructPath(x: Path): List[Node] = {
    var pathNodes = new ListBuffer[Node]()
    var n = x.point
    pathNodes.append(n)
    var pathWalker = x
    while (pathWalker.parent != null) {
//      var oldNode = pathWalker.point
      pathWalker = pathWalker.parent
      var newNode = pathWalker.point
//      newNode.setMileageTo(Converter.metersToMiles(
//        Converter.distanceHaversine((oldNode.vertex.x, oldNode.vertex.y),
//                                    (newNode.vertex.x, newNode.vertex.y))))
      pathNodes.append(newNode)
    }
    pathNodes.toList
  }

  /**
   * Estimated total cost from start to goal through y
   * @param from The node we are leaving.
   * @param goal The node we are reaching.
   * @return The total cost.
   */
  def scoreF(p: Path, candidateNode: Node, goal: Node): Double = {

    var g = scoreG(p.point, candidateNode)
    if (p.parent != null) {
      g += p.parent.g
    }
    val h = scoreH(candidateNode, goal)
    p.g = g
    p.f = g + h
    return p.f
  }

  /**
   * Cost from start along best known path.
   * @param candidateNode The node we are leaving.
   * @param toNode The node we are reaching.
   * @return The cost of the operation.
   */
  def scoreG(candidateNode: Node, toNode: Node): Double = {
              //TODO add type checking
               candidateNode.vertex.cost(toNode.vertex)
//    Converter.distanceHaversine((candidateNode.vertex.x, candidateNode.vertex.y),
//                                (toNode.vertex.x, toNode.vertex.y))
  }

  /**
   * The heuristic cost estimate; estimated cost to reach a goal node.
   * An admissible heuristic never gives a cost bigger than the real one; so
   * we use a straight line; distance Haversine
   *
   * @param candidateNode The node we are leaving.
   * @param goal The final destination node
   * @return The estimated cost to traverse the distance
   */
  def scoreH(candidateNode: Node, goal: Node): Double = {
          // todo add type checking

            candidateNode.vertex.cost(goal.vertex)
//    Converter.distanceHaversine((candidateNode.vertex.x, candidateNode.vertex.y),
//                                (goal.vertex.x, goal.vertex.y))
  }

  /**
   * Expand a path.
   *
   * @param path The path to expand.
   */
  def expand(path: Path, goal: Node): Unit = {
    var p = path.point
    var min = mindists.get(path.point)
    if (min == None || min.get.doubleValue > path.f.doubleValue) {
      mindists.put(path.point, path.f)
    }
    else {
      return
    }
    var successors: List[Node] = graph.getNeighbors(p)
    var previousPaths = reconstructPath(path)
    for (t <- successors.filter(n => !previousPaths.contains(n))) {
      val newPath = new Path(t)
      newPath.setParent(path)
      scoreF(newPath, t, goal)
      openset.offer(newPath)
    }
  }

  def compute(start: Node, goal: Node): List[Node] = {
    var root = new Path(start)
    expand(root, goal)
    openset.add(root)
    while (!openset.isEmpty) {
      var x = openset.poll()
      if (x.point == goal) {
        return reconstructPath(x)
      }
      expand(x, goal)
    }
    return List[Node]()
  }
}