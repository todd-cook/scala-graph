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
    var n = x.node
    pathNodes.append(n)
    var pathWalker = x
    while (pathWalker.parent != null) {
      pathWalker = pathWalker.parent
      var newNode = pathWalker.node
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

    var g = scoreG(p.node, candidateNode)
    if (p.parent != null) {
      g += p.parent.g
    }
    val h = scoreH(candidateNode, goal)
    p.g = g
    p.f = g + h
    return p.f
  }

  /**
   * Cost from one node to the next; used to find the best path.
   * @param candidateNode The node we are leaving.
   * @param toNode The node we are reaching.
   * @return The cost of the operation.
   */
  def scoreG(candidateNode: Node, toNode: Node): Double = {
               //candidateNode.vertex.cost(toNode.vertex)
               //graph.getEdgeCost(candidateNode.vertex, toNode.vertex)
                candidateNode.getEdges.filterNot(e => e.end == toNode.vertex)(0).getCost
  }

  /**
   * The heuristic cost estimate; estimated cost to reach a goal node.
   * An admissible heuristic never gives a cost bigger than the real one
   *
   * @param candidateNode The node we are leaving.
   * @param goal The final destination node
   * @return The estimated cost to traverse the distance
   */
  def scoreH(candidateNode: Node, goal: Node): Double = {
     //       candidateNode.vertex.cost(goal.vertex)
   //  graph.getEdgeCost(candidateNode.vertex, goal.vertex)
     candidateNode.vertex.cost(goal.vertex)
  }

  /**
   * Expand a path.
   *
   * @param path The path to expand.
   */
  def expand(path: Path, goal: Node): Unit = {
    var p = path.node
    var min = mindists.get(path.node)
    if (min == None || min.get.doubleValue > path.f.doubleValue) {
      mindists.put(path.node, path.f)
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

  /**
   * Find the shortest path
   */
  def compute(start: Node, goal: Node): List[Node] = {
    var root = new Path(start)
    expand(root, goal)
    openset.add(root)
    while (!openset.isEmpty) {
      var x = openset.poll()
      if (x.node == goal) {
        return reconstructPath(x)
      }
      expand(x, goal)
    }
    return List[Node]()
  }
}