package com.wordtrellis.scala.graphing

import org.scalatest.FlatSpec

/**
  *
  * @author : ToddCook
  *
  */
class FindPath extends FlatSpec {

  "Find path" should "print out a graph" in {
    val myGraph = new Graph()

    val node0 = new Node(Name("a"))
    val node1 = new Node(Name("b"))
    val node2 = new Node(Name("c"))
    val node3 = new Node(Name("d"))
    val node4 = new Node(Name("e"))
    val node5 = new Node(Name("f"))

    node0.addAdjacent(node3.vertex)
    node1.addAdjacent(node0.vertex)
    node2.addAdjacent(node0.vertex)
    node3.addAdjacent(node1.vertex)
    node4.addAdjacent(node1.vertex)
    node5.addAdjacent(node0.vertex)
    node0.addAdjacent(node1.vertex)
    node0.addAdjacent(node2.vertex)
    node3.addAdjacent(node3.vertex)
    node4.addAdjacent(node4.vertex)

    myGraph.addNode(node0)
    myGraph.addNode(node1)
    myGraph.addNode(node2)
    myGraph.addNode(node3)
    myGraph.addNode(node4)
    myGraph.addNode(node5)
    print(myGraph.getNodes.mkString("\n"))
  }

}
