package com.wordtrellis.scala.graphing

import java.util.PriorityQueue

import scala.collection.mutable.ListBuffer

/**
  * @author todd
  *
  */
class AStarImpl(val graph: Graph) {

  /**
    * The set of tentative nodes to be evaluated, initially containing the start node
    */
  val openset  = new PriorityQueue[Path]()
  val mindists = new collection.mutable.HashMap[Node, Double]()

  val radianConversion: Double = 180d / java.lang.Math.PI

  //diagnostic methods
  def getNumberOfPaths: Int = openset.size()

  def getMinDistancesSize: Int = mindists.size

  /**
    * Find the shortest path
    */
  def compute(start: Node, goal: Node): List[Node] = {
    val root = new Path(start)
    expand(root, goal)
    openset.add(root)
    while (!openset.isEmpty) {
      val x = openset.poll()
      if (x.node == goal) {
        return reconstructPath(x)
      }
      expand(x, goal)
    }
    List[Node]()
  }

  /**
    * Expand a path.
    *
    * @param path The path to expand.
    */
  def expand(path: Path, goal: Node): Unit = {
    val p   = path.node
    val min = mindists.get(path.node)
    if (min.isEmpty || min.get.doubleValue > path.f.doubleValue) {
      mindists.put(path.node, path.f)
    } else {
      return
    }
    val successors: List[Node] = graph.getNeighbors(p)
    val previousPaths          = reconstructPath(path)
    for (t <- successors.filter(n => !previousPaths.contains(n))) {
      val newPath = new Path(t)
      newPath.setParent(path)
      scoreF(newPath, t, goal)
      openset.offer(newPath)
    }
  }

  def reconstructPath(x: Path): List[Node] = {
    val pathNodes = new ListBuffer[Node]()
    val n         = x.node
    pathNodes.append(n)
    var pathWalker = x
    while (pathWalker.parent != null) {
      pathWalker = pathWalker.parent
      val newNode = pathWalker.node
      pathNodes.append(newNode)
    }
    pathNodes.toList
  }

  /**
    * Estimated total cost from start to goal through y
    *
    * @param p             the path object
    * @param candidateNode The node we are leaving.
    * @param goal          The node we are reaching.
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
    p.f
  }

  /**
    * Cost from one node to the next; used to find the best path.
    *
    * @param candidateNode The node we are leaving.
    * @param toNode        The node we are reaching.
    * @return The cost of the operation.
    */
  def scoreG(candidateNode: Node, toNode: Node): Double = {
    //candidateNode.vertex.cost(toNode.vertex)
    //graph.getEdgeCost(candidateNode.vertex, toNode.vertex)
    candidateNode.getEdges.filterNot(e => e.end == toNode.vertex).head.getCost
  }

  /**
    * The heuristic cost estimate; estimated cost to reach a goal node.
    * An admissible heuristic never gives a cost bigger than the real one
    *
    * @param candidateNode The node we are leaving.
    * @param goal          The final destination node
    * @return The estimated cost to traverse the distance
    */
  def scoreH(candidateNode: Node, goal: Node): Double = {
    //       candidateNode.vertex.cost(goal.vertex)
    //  graph.getEdgeCost(candidateNode.vertex, goal.vertex)
    candidateNode.vertex.cost(goal.vertex)
  }
}
