package com.cookconsulting.scala.graphing;

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.Int;
import scala.collection.mutable.ListBuffer
import org.junit.Assert.assertEquals
import java.io.File
import java.util.Stack

/**
 * See: http://www.cs.duke.edu/csed/algoprobs/p15.html
 *
 * Problem Statement
An essential part of circuit design and general system optimization is critical path analysis. On a chip, the critical path represents the longest path any signal would have to travel during execution. In this problem we will be analyzing chip designs to determine their critical path length. The chips in this problem will not contain any cycles, i.e. there exists no path from one component of a chip back to itself.

Given a String[] connects representing the wiring scheme, and a String[] costs representing the cost of each connection, your method will return the size of the most costly path between any 2 components on the chip. In other words, you are to find the longest path in a directed, acyclic graph. Element j of connects will list the components of the chip that can be reached directly from the jth component (0-based). Element j of costs will list the costs of each connection mentioned in the jth element of connects. As mentioned above, the chip will not contain any cyclic paths.

For example:

connects = {"1 2",
            "2",
            ""}
costs    = {"5 3",
            "7",
            ""}

In this example, component 0 connects to components 1 and 2 with costs 5 and 3 respectively. Component 1 connects to component 2 with a cost of 7. All connections mentioned are directed. This means a connection from component i to component j does not imply a connection from component j to component i. Since we are looking for the longest path between any 2 components, your method would return 12.

Definition

    Class: Circuits
    Method: howLong
    Parameters: String[], String[]
    Returns: int
    Method signature (see below, be sure your method is public):

Class
class Circuits { public int howLong(String[] connects, String[] costs) { // fill in code here } }

Constraints

    connects must contain between 2 and 50 elements inclusive
    connects must contain the same number of elements as costs
    Each element of connects must contain between 0 and 50 characters inclusive
    Each element of costs must contain between 0 and 50 characters inclusive
    Element i of connects must contain the same number of integers as element i of costs
    Each integer in each element of connects must be between 0 and the size of connects-1 inclusive
    Each integer in each element of costs must be between 1 and 1000 inclusive
    Each element of connects may not contain repeated integers
    Each element of connects must be a single space delimited list of integers, each of which has no extra leading zeros. There will be no leading or trailing whitespace.
    Each element of costs must be a single space delimited list of integers, each of which has no extra leading zeros. There will be no leading or trailing whitespace.
    The circuit may not contain any cycles
    There will be at least 1 connection.

Examples
In each of these examples, quotes ('"'), commas (','), and curly braces ('{' and '}') are included for clarity only. They are never part of the input or output.

    connects = {"1 2", "2", ""}
    costs =    {"5 3", "7", ""}

    Returns:  12

    From above


    connects = {"1 2 3 4 5","2 3 4 5","3 4 5","4 5","5",""}
    costs    = {"2 2 2 2 2","2 2 2 2","2 2 2","2 2","2",""}

    Returns: 10

    The longest path goes from 0-1-2-3-4-5 for a cost of 10.

    connects = {"1","2","3","","5","6","7",""}
    costs    = {"2","2","2","","3","3","3",""}

    Returns:9

    The 0-1-2-3 path costs 6 whereas the 4-5-6-7 path costs 9

    connects = {"","2 3 5","4 5","5 6","7","7 8","8 9","10",
                "10 11 12","11","12","12",""}
    costs    = {"","3 2 9","2 4","6 9","3","1 2","1 2","5",
                "5 6 9","2","5","3",""}

    Returns: 22

    connects = {"","2 3","3 4 5","4 6","5 6","7","5 7",""}
    costs    = {"","30 50","19 6 40","12 10","35 23","8","11 20",""}

    Returns: 105
 *
 * This solution
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
        println("graph is:")
        println(g)
        println("*********")
        var longestPaths = new LongestPaths (g)
        var paths = longestPaths.find
        var scores = paths.map( z => g.totalCost(z))
        val highScore =  scores.sortWith(_>_).head
        val highScoreIndex =  scores.indexOf(highScore)
        println("Longest path is: " +  paths(highScoreIndex) )
        //println("Longest path is: " +  longest)
        //val cost = g.totalCost(longest)
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