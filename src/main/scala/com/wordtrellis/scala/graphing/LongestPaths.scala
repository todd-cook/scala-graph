package com.wordtrellis.scala.graphing

import scala.collection.mutable.ListBuffer

/**
  *
  * @author : Todd Cook
  *
  */
class LongestPaths(val graph: Graph) {

  val paths = new ListBuffer[List[Node]]()
  var path  = new ListBuffer[Node]()

  def find(): List[List[Node]] = {
    graph.getNodes.foreach(n => {
      n.setState(NodeState.VISITED)
      createPaths(List(n))
      n.setState(NodeState.UNVISITED)
      //optional trim after cycle to on longest list per node
    })
    if (paths.isEmpty) return List[List[Node]]()
    //println("buffer is: ")
    // paths.foreach(p => println(p.mkString(",")))
    //println("*****")
    val longest = paths.sortBy(_.length).toList(paths.size - 1)
    paths.toList.filter(z => z.length == longest.length)
  }

  def createPaths(nodeList: List[Node]): Unit = {
    nodeList.last.getAdjacents.foreach(v => {
      val n = graph.find(v)
      if (n.isDefined && n.get.getState != NodeState.VISITED) {
        val newList = nodeList ::: List(n.get)
        if (!paths.contains(newList)) {
          paths.append(newList)
          createPaths(newList)
        }
      }
    })
  }
}
