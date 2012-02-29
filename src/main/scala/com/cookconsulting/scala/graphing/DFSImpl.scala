package com.cookconsulting.scala.graphing;

import scala.collection.mutable.ListBuffer;


import java.util.Stack;

/**
 *    TODO clean up or delete this file
 * @author : ToddCook
 * @since : 2/23/12 12:28 PM
 */

class DFSImpl (val graph:Graph ) {

    var stack = new Stack[Node] ()
        var paths = new ListBuffer[List[Node]]()
               var path = new ListBuffer[Node]()

//        def init(){
//      graph.getNodes.foreach(n =>
//        n.setState(NodeState.INITIAL)
//        dfs(n)
//        )
//        }

   def dfs(node:Node)  :Int ={

     //   node.setState(NodeState.VISITING)
          node.getAdjacents().foreach( v => {
          var n = graph.find(v).get
          if(n.getState == NodeState.UNVISITED) {
                n.setState(NodeState.VISITING)
                stack.push(n)
                dfs (n)
                }
          })
        node.setState(NodeState.VISITED)
                             0
   }

    def dfsMaxChain (node: Node ) :List[Node] = {

           if (node.getAdjacents().length == 0)
               return List(node)

           var lists = new ListBuffer[List[Node]]()
           stack.push(node)
           while(!stack.empty()){
               var buf = new ListBuffer[Node]()
                   var top = stack.peek ();
               top.setState (NodeState.VISITED)
               top.getAdjacents.foreach( a => stack.add(graph.find(a).get))
  //                 stack.addAll(top.getAdjacents)
               top.setState (NodeState.UNVISITED)
                stack.pop()


           }
           // find the longest list in the collection and return it
          return List[Node]()

       }


}