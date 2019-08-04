package com.wordtrellis.scala.graphing

import scala.collection.mutable.ListBuffer

/**
  *
  * @author : ToddCook
  *
  */

class BFSImpl(val graph: Graph) {

  val stack = new java.util.Stack[Node]()
  //  var paths = new ListBuffer[List[Node]]()
  //  var path = new ListBuffer[Node]()


  def bfs(node: Node): Int = {

    node.getAdjacents.foreach(v => {
      val n = graph.find(v).get
      if (n.getState == NodeState.UNVISITED) {
        n.setState(NodeState.VISITING)
        stack.push(n)
        bfs(n)
      }
    })
    node.setState(NodeState.VISITED)
    0
  }

  def bfsMaxChain(node: Node): List[Node] = {

    if (node.getAdjacents.isEmpty)
      return List(node)

    var lists = new ListBuffer[List[Node]]()
    stack.push(node)
    while (!stack.empty()) {
      var buf = new ListBuffer[Node]()
      val top = stack.peek()
      top.setState(NodeState.VISITED)
      top.getAdjacents.foreach(a => stack.add(graph.find(a).get))
      top.setState(NodeState.UNVISITED)
      stack.pop()
    }
    // find the longest list in the collection and return it
    List[Node]()
  }
}