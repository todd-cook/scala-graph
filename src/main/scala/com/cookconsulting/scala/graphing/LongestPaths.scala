package com.cookconsulting.scala.graphing;

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer

/**
 *  
 * @author : ToddCook
 * @since : 2/23/12 4:11 PM
 */
class LongestPaths(val graph:Graph) {

    var paths = new ListBuffer[List[Node]]()
    var path = new ListBuffer[Node]()

    def find() :List[List[Node]] ={
        graph.getNodes.foreach( n => {
        n.setState(NodeState.VISITED)
        createPaths( List(n))
        n.setState(NodeState.UNVISITED)
        //optional trim after cycle to on longest list per node
        })
        if (paths.size ==0) return List[ List[Node]]()
        //println("buffer is: ")
       // paths.foreach(p => println(p.mkString(",")))
        //println("*****")
         val longest = paths.sortBy(_.length).toList(paths.size -1)
        paths.toList.filter(z => z.length  ==  longest.length)
    }


    def createPaths (nodeList :List[Node]) :Unit={
        nodeList.last.getAdjacents.foreach( v =>{
        var n = graph.find(v)
        if ( n !=  None &&  n.get.getState != NodeState.VISITED) {
            var newList = nodeList ::: List(n.get)
            if (!paths.contains(newList)){
                paths.append(newList)
                createPaths(newList)
                }
        }
        }
        )
    }
}


//        def generate(){
//            var nodes = acyclic.getNodes
//        nodes.foreach(n =>
//        n.setState(NodeState.VISITING)
//        path.append(n)
//        var neighbors = n.getNeighbors
//
//        buildPaths(n))
//
//        }
//
//        def buildPaths (n:Node) ={
//        }
