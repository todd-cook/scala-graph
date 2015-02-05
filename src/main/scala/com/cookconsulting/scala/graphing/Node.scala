package com.cookconsulting.scala.graphing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

/**
 * Graph Objects
 *
 */


trait Vertex  {
        def cost (other: Vertex) :Double =1D
        }

class XY (val x:Int, val y:Int) extends Vertex {
        /* Taxicab/"Manhattan" distance */
        override def cost (other:Vertex) :Double ={
        (Converter.taxicabDistance((x,y),
        (other.asInstanceOf[XY].x, other.asInstanceOf[XY].y))).asInstanceOf[Double]
        }
        override def toString() = "(" + x + ", " + y + ")"

        override def hashCode = x.hashCode * 41 * y.hashCode

        def canEqual(other: Any) = other.isInstanceOf[XY]

        override def equals(other: Any) = other match {
          case that: XY => (that canEqual this) && (this.x == that.x) && (this.y == that.y)
          case _ => false
        }

        }

case class LatLon (val lat:Double, val lon:Double) extends Vertex {
        override def cost (other:Vertex) :Double ={
        Converter.distanceHaversine(  (lat, lon),( other.asInstanceOf[LatLon].lat, other.asInstanceOf[LatLon].lon))
        }
        }
case class Point (val name :Int) extends Vertex
case class Name (val name: String) extends Vertex


class Edge(val start: Vertex, val end: Vertex, units: String = "latitude/longitude") {
  var cost: Double =1d // start.cost(end) // 0D

  def setCost(d: Double) {
    cost = d
  }

  def getCost() = cost

  override def toString(): String = "start: " + start + " end: " + end + " cost: " + cost

    override def hashCode = start.hashCode  *41 * end.hashCode
    def canEqual(other: Any) = other.isInstanceOf[Edge]
    override def equals(other: Any) = other match {
      case that: Edge => (that canEqual this) && (this.start == that.start) && (this.end == that.end)
      case _ => false
    }
}

object NodeState extends Enumeration {
         type NodeState = Value
         val UNVISITED, VISITED, VISITING , INITIAL = Value
     }

// must bring the enum object in...
import NodeState._

class Node(val vertex: Vertex,
           var name: String = "", var domain: String = "",
           val units: Option[String] = None) {

  private var edges = List[Edge]()
  private var adjacents = List[Vertex]()

  def addEdge(newEdge: Edge) {
    edges = edges ::: List(newEdge)
//        if (newEdge.start == this.vertex)
//        addAdjacent(newEdge.end)
//        else
//        addAdjacent(newEdge.start)
  }

  def getCostTo(toNode:Node) :Double = {
      var can = edges.find(_.end == toNode.vertex)
      if (can == None) return Integer.MAX_VALUE.toDouble
      can.get.getCost
  }
  
  // todo add remove edge?
  def removeEdge(e :Edge) { edges = edges.filterNot(_ == e) }
  def removeVertex(v :Vertex) { adjacents = adjacents.filterNot(_ == v) }

  def getEdges() = edges

  def addAdjacent(v: Vertex) {
    adjacents = adjacents ::: List(v)
  }
  def getAdjacents(): List[Vertex] = adjacents

  private var state = NodeState.UNVISITED
 def getState() = state
 def setState( s:NodeState) { state = s }

  override def toString(): String = {
    "Node: " + name + " " + vertex.toString +
        "\n  adjacents:    \n" + adjacents.mkString("\n") +
        "\n  edges:    \n" + edges.mkString("\n")
  }
  override def hashCode = vertex.hashCode
  def canEqual(other: Any) = other.isInstanceOf[Node]
  override def equals(other: Any) = other match {
    case that: Node => (that canEqual this) && (this.vertex == that.vertex )
    case _ => false
  }
}

class Path(val node: Node) extends Ordered[Path] {
  var f = 0D
  var g = 0D
  var parent: Path = null;

  def setF(score: Double) {
    f = score
  }

  def setG(score: Double) {
    g = score
  }

  def setParent(p: Path) {
    parent = p
  }

  def compare(that: Path) = this.f.compareTo(that.f)

  override def toString(): String = {
    if (parent != null) {
      return node.vertex.toString() + " -> " + parent.toString()
    }
    else {
      return node.vertex.toString()
    }
  }
}


class Route (val nodes :List[Node]){
        def start = nodes(0)
        def end = nodes(nodes.length -1)
        val distances = createDistances()
        val totalDistance = distances.sum
        def createDistances() =		{
        if (nodes(0).vertex.isInstanceOf[LatLon]) {
				val vertices = nodes.map (a => (a.vertex.asInstanceOf[LatLon].lat, a.vertex.asInstanceOf[LatLon].lon))
				((vertices.slice(1, vertices.length)).zip(vertices.slice(0, vertices.length -1))).map( a =>
                    Converter.metersToMiles(Converter.distanceHaversine(( a._1._1, a._1._2 ), (  a._2._1, a._2._2  ))))
					}
        else  List()
					}

        override def toString () = getRouteList().mkString("\n")
        def getRouteList() = {
        val buf =  new ListBuffer[String]()
           Iterator.range(0, distances.length).foreach( i => {
                   buf.append(nodes(i).name + "\tto\t" + nodes(i+1 ).name + "\t"
                   + "(" + String.format("%.2f", distances(i).asInstanceOf[Object]) +" miles)")
           })
           buf.append ("Total path mileage: " + java.lang.String.format("%.2f", distances.sum.asInstanceOf[Object]))
           if (nodes(0).vertex.isInstanceOf[LatLon]) {

           buf.append("Distance as the crow flies: " + String.format("%.2f",
                   Converter.metersToMiles(Converter.distanceHaversine(
                           (nodes(0).vertex.asInstanceOf[LatLon].lat, nodes(0).vertex.asInstanceOf[LatLon].lon),
                           (nodes(nodes.length -1).vertex.asInstanceOf[LatLon].lat,
        nodes(nodes.length -1).vertex.asInstanceOf[LatLon].lon))).asInstanceOf[Object]))
        }
         buf.toList
        }
}


class Graph {

  // representing nodes as a list is way too slow, need constant access of hash map
  var nodes = new HashMap[Vertex, Node]() 

  def getNodes(): List[Node] = nodes.values.toList
  
  def getNode(v:Vertex) :Node =  nodes.get(v).get
  
  def addNode(n: Node) =  nodes.put(n.vertex, n )

  override def toString(): String = getNodes.mkString("\n")

  def find(v: Vertex): Option[Node] = nodes.get(v) 

  def getVertices(): List[Vertex] = nodes.keys.toList

  def getNeighbors(n: Node): List[Node] = n.getAdjacents().map(v => find(v).get).toList

  def getEdges() :List[Edge] = (nodes.values.toList.map( n => n.getEdges)).flatten

  def totalCost (nodeList :List[Node]) :Double  ={
        val costs = new HashMap[ Tuple2[Vertex,Vertex], Double]()
      getEdges.foreach(e => costs.put( (e.start, e.end), e.getCost))
     // costs.keys.foreach( k=> print( costs.get(k).get))      
      val vertices = nodeList.map (a => a.vertex )
      val nodeListEdges = ((vertices.slice(0, vertices.length -1)).zip(
          vertices.slice(1, vertices.length))).map(x =>costs.get((x._1, x._2)))  
/*      vertices.slice(1, vertices.length))).map(x =>{
              var tmpCost =costs.get((x._1, x._2))
              if (tmpCost.get.toInt == Integer.MAX_VALUE){
              println("warning score Out of range: " + (x._1, x._2))
              }
                tmpCost } )
                */
      nodeListEdges.flatten.sum
    }

    def setCostForEdge (startVertex :Vertex, endVertex:Vertex, cost:Double) :Unit ={
	var n = find(endVertex)
	if (n ==None) return
    //println("setting cost for " + n.get)
	//var edges = n.get.getEdges
	var nodesToSet = getNeighbors(n.get) //edges.map(_.start).map(a => find(a)).flatten
   // println("nodesToSet: " + nodesToSet)
	nodesToSet.foreach(n => n.getEdges().foreach( e => if (e.end == endVertex && e.start == startVertex ) e.setCost(cost)))
    }
    
    def getEdgeCost(startVertex :Vertex, endVertex:Vertex) :Double = {
	var n = find(endVertex)
	if (n ==None) return Integer.MAX_VALUE.toDouble
    //println("setting cost for " + n.get)
	//var edges = n.get.getEdges
	var nodes = getNeighbors(n.get) //edges.map(_.start).map(a => find(a)).flatten
   // println("nodesToSet: " + nodesToSet)
	nodes.foreach(n => n.getEdges().foreach( e => if (e.end == endVertex && e.start == startVertex ) return e.getCost ))
     return Integer.MAX_VALUE.toDouble
    }

    ///def getEdge( from:Node, to:Node) = 
    
}






