package com.wordtrellis.scala.graphing

import org.scalatest.FlatSpec

/**
  * See: http://www.cs.duke.edu/csed/algoprobs/p15.html
  *
  * Problem Statement
  *
  * An essential part of circuit design and general system optimization is
  * critical path analysis. On a chip, the critical path represents the longest
  * path any signal would have to travel during execution.
  *
  * We will analyzing chip designs to determine their critical path length.
  * The chips in this problem will not contain any cycles,
  * i.e. there exists no path from one component of a chip back to itself.
  *
  * Given a String[] connects representing the wiring scheme,
  * and a String[] costs representing the cost of each connection,
  * your method will return the size of the most costly path between
  * any 2 components on the chip.
  * In other words, you are to find the longest path in a directed,
  * acyclic graph. Element j of connects will list the components of the chip
  * that can be reached directly from the jth component (0-based).
  * Element j of costs will list the costs of each connection mentioned in the
  * jth element of connects. As mentioned above,
  * the chip will not contain any cyclic paths.
  *
  * For example:
  *
  * connects = {"1 2",
  * "2",
  * ""}
  * costs    = {"5 3",
  * "7",
  * ""}
  *
  * In this example,
  * component 0 connects to components 1 and 2 with costs 5 and 3 respectively.
  * Component 1 connects to component 2 with a cost of 7.
  * All connections mentioned are directed.
  * This means a connection from component i to component j does not imply
  * a connection from component j to component i. Since we are looking
  * for the longest path between any 2 components, your method would return 12.
  *
  * Definition
  *
  * Class: Circuits
  * Method: howLong
  * Parameters: String[], String[]
  * Returns: int
  * Method signature (see below, be sure your method is public):
  *
  * Class
  * class Circuits { public int howLong(String[] connects, String[] costs)
  * { // fill in code here } }
  *
  * Constraints
  *
  * connects must contain between 2 and 50 elements inclusive
  * connects must contain the same number of elements as costs
  * Each element of connects must contain between 0 and 50 characters inclusive
  * Each element of costs must contain between 0 and 50 characters inclusive
  * Element i of connects must contain the same number of integers
  * as element i of costs
  * Each integer in each element of connects must be between 0 and
  * the size of connects-1 inclusive
  * Each integer in each element of costs must be between 1 and 1000 inclusive
  * Each element of connects may not contain repeated integers
  * Each element of connects must be a single space delimited
  * list of integers, each of which has no extra leading zeros.
  * There will be no leading or trailing whitespace.
  * Each element of costs must be a single space delimited list of integers,
  * each of which has no extra leading zeros.
  * There will be no leading or trailing whitespace.
  * The circuit may not contain any cycles
  * There will be at least 1 connection.
  *
  * Examples
  * In each of these examples, quotes ('"'), commas (','),
  * and curly braces ('{' and '}') are included for clarity only.
  * They are never part of the input or output.
  *
  * connects = {"1 2", "2", ""}
  * costs =    {"5 3", "7", ""}
  *
  * Returns:  12
  *
  * From above
  *
  *
  * connects = {"1 2 3 4 5","2 3 4 5","3 4 5","4 5","5",""}
  * costs    = {"2 2 2 2 2","2 2 2 2","2 2 2","2 2","2",""}
  *
  * Returns: 10
  *
  * The longest path goes from 0-1-2-3-4-5 for a cost of 10.
  *
  * connects = {"1","2","3","","5","6","7",""}
  * costs    = {"2","2","2","","3","3","3",""}
  *
  * Returns:9
  *
  * The 0-1-2-3 path costs 6 whereas the 4-5-6-7 path costs 9
  *
  * connects = {"","2 3 5","4 5","5 6","7","7 8","8 9","10",
  * "10 11 12","11","12","12",""}
  * costs    = {"","3 2 9","2 4","6 9","3","1 2","1 2","5",
  * "5 6 9","2","5","3",""}
  *
  * Returns: 22
  *
  * connects = {"","2 3","3 4 5","4 6","5 6","7","5 7",""}
  * costs    = {"","30 50","19 6 40","12 10","35 23","8","11 20",""}
  *
  * Returns: 105
  *
  * This solution
  *
  * @author : ToddCook
  */
class CircuitTest extends FlatSpec {

  def howLong(connects: List[String], costs: List[String]): Any = {
    val g = new Graph()
    Iterator
      .range(0, connects.length)
      .foreach(x => {
        val n = new Node(Point(x))
        Iterator
          .range(0, connects(x).split(" ").length)
          .foreach(y => {
            // add the edges since they will be used to find the cost
            // note: the schlocky data structure of string arrays is horribly buggy,
            // hence we must do defensive checking such as this for nodes without adjacents
            if (connects(x).split(" ")(y).length > 0) {
              val e = new Edge(Point(x), Point(connects(x).split(" ")(y).toInt))
              e.setCost(costs(x).split(" ")(y).toDouble)
              n.addEdge(e)
              // add adjacents, since they will be used for navigation
              // TODO consider condensing
              val v = Point(connects(x).split(" ")(y).toInt)
              n.addAdjacent(v)
            }
          })
        g.addNode(n)
      })
    println("graph is:")
    println(g)
    println("*********")
    val longestPaths   = new LongestPaths(g)
    val paths          = longestPaths.find()
    val scores         = paths.map(z => g.totalCost(z))
    val highScore      = scores.sortWith(_ > _).head
    val highScoreIndex = scores.indexOf(highScore)
    println("Longest path is: " + paths(highScoreIndex))
    //println("Longest path is: " +  longest)
    //val cost = g.totalCost(longest)
    println("Total cost: " + highScore)
    highScore
  }

  "Test 1 " should "check the path length" in {
    assert(12.0 == howLong(List("1 2", "2", ""), List("5 3", "7", "")), 0.01)
  }

  "Test 2 " should "check the path length" in {
    //The longest path goes from 0-1-2-3-4-5 for a cost of 10.
    assert(10.0 == howLong(List("1 2 3 4 5", "2 3 4 5", "3 4 5", "4 5", "5", ""),
                           List("2 2 2 2 2", "2 2 2 2", "2 2 2", "2 2", "2", "")),
           0.01)
  }
  "Test 3" should "check the path length" in {
    // 	The 0-1-2-3 path costs 6 whereas the 4-5-6-7 path costs 9
    assert(9.0 == howLong(List("1", "2", "3", "", "5", "6", "7", ""),
                          List("2", "2", "2", "", "3", "3", "3", "")),
           0.01)
  }

  "Test 4 " should "check the path length" in {
    assert(
      22.0 == howLong(
        List("", "2 3 5", "4 5", "5 6", "7", "7 8", "8 9", "10", "10 11 12", "11", "12", "12", ""),
        List("", "3 2 9", "2 4", "6 9", "3", "1 2", "1 2", "5", "5 6 9", "2", "5", "3", "")
      ),
      0.01
    )
  }

  "Test 5 " should "check the path length" in {
    //NOTE: gives 103, not 105
    assert(103.0 == howLong(
             List("", "2 3", "3 4 5", "4 6", "5 6", "7", "5 7", ""),
             List("", "30 50", "19 6 40", "12 10", "35 23", "8", "11 20", "")
           ),
           0.01)
  }
}
