package com.cookconsulting.scala.graphing;

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.Int;
import scala.collection.mutable.ListBuffer
import org.junit.Assert.assertEquals
import java.io.File
import java.util.Stack

/**
 *  
 * @author : ToddCook
 * @since : 2/23/12 5:35 PM
 */

class FindPath  extends AssertionsForJUnit {

    @Test
    def testBuildGraph() {

        val myGraph = new Graph()

        val node0 = new Node(new Name("a")  )
        val node1 = new Node(new Name("b")  )
        val node2 = new Node(new Name("c")  )
        val node3 = new Node(new Name("d")  )
        val node4 = new Node(new Name("e")  )
        val node5 = new Node(new Name("f")  )

        node0.addAdjacent( node3.vertex )
        node1.addAdjacent( node0.vertex )
        node2.addAdjacent( node0.vertex )
        node3.addAdjacent( node1.vertex )
        node4.addAdjacent( node1.vertex )
        node5.addAdjacent( node0.vertex )
        node0.addAdjacent( node1.vertex )
        node0.addAdjacent( node2.vertex )
        node3.addAdjacent( node3.vertex )
        node4.addAdjacent( node4.vertex )

        myGraph.addNode(node0)
        myGraph.addNode(node1)
        myGraph.addNode(node2)
        myGraph.addNode(node3)
        myGraph.addNode(node4)
        myGraph.addNode(node5)
        print(myGraph.getNodes.mkString("\n"))
    }


}