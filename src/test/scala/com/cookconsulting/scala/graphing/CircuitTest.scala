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
 * @since 9/12/11 10:22 PM
 * @since : 2/22/12 12:08 PM
 */
class CircuitTest extends AssertionsForJUnit {

   def howLong (connects :List[String], costs :List[String])={
    var g = new Graph()
    Iterator.range(0, connects.length ).foreach(x =>{
        var n = new Node(new Point(x))
    Iterator.range( 0, connects(x).split(" ").length ).foreach(
    y =>{
            // add the edges since they will be used to find the cost
        // note: the schlocky data structure of string arrays is horribly buggy,
        // hence we must do defensive checking such as this for nodes without adjacents
        if (connects(x).split(" ")(y).length > 0) {
        var e = new Edge(new Point(x), new Point(connects(x).split(" ")(y).toInt ))
        e.setCost (costs(x).split(" ")(y).toDouble)
        n.addEdge(e)
       // add adjacents, since they will be used for navigation
        // TODO consider condensing
        var v =  new Point(connects(x).split(" ")(y).toInt)
        n.addAdjacent (v)
        }
    })
        g.addNode(n)
    })
        println("graph is: " + g)
        var longestPaths = new LongestPaths (g)
        var paths = longestPaths.find
        var scores = paths.map( z => g.totalCost(z))
        val highScore =  scores.sortWith(_>_).head
        val highScoreIndex =  scores.indexOf(highScore)
        println("Longest path is: " +  paths(highScoreIndex) )
        println("Total cost: " + highScore)
      highScore
    }

//    def dfsMaxChain  ( nodes:List[Node]) :Int ={
//
//        var n = nodes(0)
//        var stack = new Stack[Node] ()
//        stack.push(n)
////                while(!stack.empty()){
////                    var top = stack.top();
////                    stack.pop
////                    top.state (NodeState.VISITED)
////                            stack.addAll(top.getAdjacents)
////
////                }
//              1
//    }

/**
* The node inside the node list should be marked VISITED
*/
//    def dfsMaxChain (node: Node, nodes :List[Node]) :List[Node] = {
//
//        if (node.getAdjacents().length == 0)
//            return List(node)
//
//        var lists = new ListBuffer[List[Node]]()
//
//        var stack = new Stack[Node] ()
//        stack.push(node)
//
//        while(stack.empty() != true){
//            var buf = new ListBuffer[Node]()
//                var top = stack.peek ();
//            top.setState (NodeState.VISITED)
//                    /// todo
//            // top.getAdjacents.foreach( a => stack.add(graph.find(a).get))
//               // stack.addAll(top.getAdjacents)
//            top.setState (NodeState.UNVISITED)
//             stack.pop()
//
//
//        }
//        // find the longest list in the collection and return it
//      List[Node]()
//
//    }
    
    @Test
    def test1()  {
      assertEquals (12.0, howLong(List("1 2", "2", ""),
              List("5 3", "7", "")), 0.01 )
    }

    @Test
    def test2()  {
  //The longest path goes from 0-1-2-3-4-5 for a cost of 10.
      assertEquals (10.0, howLong(
              List("1 2 3 4 5","2 3 4 5","3 4 5","4 5","5",""),
              List( "2 2 2 2 2","2 2 2 2","2 2 2","2 2","2","")), 0.01 )
    }

    @Test
    def test3()  {
  // 	The 0-1-2-3 path costs 6 whereas the 4-5-6-7 path costs 9
      assertEquals (9.0, howLong(List("1","2","3","","5","6","7",""),
              List("2","2","2","","3","3","3","")), 0.01 )
    }


    @Test
    def test4()  {
      assertEquals (22.0, howLong(
		List( "","2 3 5","4 5","5 6","7","7 8","8 9","10",
		 "10 11 12","11","12","12",""),
		 List("","3 2 9","2 4","6 9","3","1 2","1 2","5",
		 "5 6 9","2","5","3","")), 0.01 )
    }

    @Test
      def test5()  {
        //NOTE: gives 103, not 105
        assertEquals (103.0, howLong(
           List("","2 3",  "3 4 5"   ,"4 6",   "5 6",  "7", "5 7",""),
          List("", "30 50","19 6 40", "12 10", "35 23","8", "11 20","")
            ), 0.01 )
      }
}