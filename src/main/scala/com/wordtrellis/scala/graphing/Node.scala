package com.wordtrellis.scala.graphing

import com.wordtrellis.scala.graphing

import scala.collection.mutable.ListBuffer

/**
  * Graph Objects
  */

trait Vertex {
  def cost(other: Vertex): Double = 1D
}

case class XY(x: Int, y: Int) extends Vertex {
  /* Taxicab/"Manhattan" distance */
  override def cost(other: Vertex): Double = {
    Converter.taxicabDistance((x, y),
      (other.asInstanceOf[XY].x, other.asInstanceOf[XY].y)).asInstanceOf[Double]
  }

  override def toString: String = "(" + x + ", " + y + ")"

  override def hashCode: Int = x.hashCode * 41 * y.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: XY => (that canEqual this) && (this.x == that.x) && (this.y == that.y)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[XY]
}

case class LatLon(lat: Double, lon: Double) extends Vertex {
  override def cost(other: Vertex): Double = {
    Converter.distanceHaversine((lat, lon), (other.asInstanceOf[LatLon].lat, other.asInstanceOf[LatLon].lon))
  }
}

case class Point(name: Int) extends Vertex

case class Name(name: String) extends Vertex


class Edge(val start: Vertex, val end: Vertex, units: String = "latitude/longitude") {
  var cost: Double = 1d

  def getCost: Double = cost

  def setCost(d: Double) {
    cost = d
  }

  override def toString: String = "start: " + start + " end: " + end + " cost: " + cost

  override def hashCode: Int = start.hashCode * 41 * end.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: Edge => (that canEqual this) && (this.start == that.start) && (this.end == that.end)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Edge]
}

object NodeState extends Enumeration {
  type NodeState = Value
  val UNVISITED, VISITED, VISITING, INITIAL = Value
}

// must bring the enum object in...

import com.wordtrellis.scala.graphing.NodeState._

class Node(val vertex: Vertex,
           var name: String = "", var domain: String = "",
           val units: Option[String] = None) {

  private var edges = List[Edge]()
  private var adjacents = List[Vertex]()
  private var state = NodeState.UNVISITED

  def addEdge(newEdge: Edge) {
    edges = edges ::: List(newEdge)
  }

  def getCostTo(toNode: Node): Double = {
    val can = edges.find(_.end == toNode.vertex)
    if (can.isEmpty) return Integer.MAX_VALUE.toDouble
    can.get.getCost
  }

  def removeEdge(e: Edge) {
    edges = edges.filterNot(_ == e)
  }

  def removeVertex(v: Vertex) {
    adjacents = adjacents.filterNot(_ == v)
  }

  def getEdges: List[Edge] = edges

  def addAdjacent(v: Vertex) {
    adjacents = adjacents ::: List(v)
  }

  def getAdjacents: List[Vertex] = adjacents

  def getState: graphing.NodeState.Value = state

  def setState(s: NodeState) {
    state = s
  }

  override def toString: String = {
    "Node: " + name + " " + vertex.toString +
      "\n  adjacents:    \n" + adjacents.mkString("\n") +
      "\n  edges:    \n" + edges.mkString("\n")
  }

  override def hashCode: Int = vertex.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: Node => (that canEqual this) && (this.vertex == that.vertex)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]
}

class Path(val node: Node) extends Ordered[Path] {
  var f = 0D
  var g = 0D
  var parent: Path = _

  def setF(score: Double) {
    f = score
  }

  def setG(score: Double) {
    g = score
  }

  def setParent(p: Path) {
    parent = p
  }

  def compare(that: Path): Int = this.f.compareTo(that.f)

  override def toString: String = {
    if (parent != null) {
      node.vertex.toString + " -> " + parent.toString
    }
    else {
      node.vertex.toString
    }
  }
}


class Route(val nodes: List[Node]) {
  val distances = createDistances()
  val totalDistance = distances.sum

  def start = nodes.head

  def end: Node = nodes.last

  def createDistances() = {
    if (nodes.head.vertex.isInstanceOf[LatLon]) {
      val vertices = nodes.map(a => (a.vertex.asInstanceOf[LatLon].lat, a.vertex.asInstanceOf[LatLon].lon))
      vertices.slice(1, vertices.length).zip(vertices.slice(0, vertices.length - 1)).map(a =>
        Converter.metersToMiles(Converter.distanceHaversine((a._1._1, a._1._2), (a._2._1, a._2._2))))
    }
    else List()
  }

  override def toString: String = getRouteList.mkString("\n")

  def getRouteList: List[String] = {
    val buf = new ListBuffer[String]()
    Iterator.range(0, distances.length).foreach(i => {
      buf.append(nodes(i).name + "\tto\t" + nodes(i + 1).name + "\t"
        + "(" + String.format("%.2f", distances(i).asInstanceOf[Object]) + " miles)")
    })
    buf.append("Total path mileage: " + java.lang.String.format("%.2f", distances.sum.asInstanceOf[Object]))
    nodes.head.vertex match {
      case lon: LatLon =>

        buf.append("Distance as the crow flies: " + String.format("%.2f",
          Converter.metersToMiles(Converter.distanceHaversine(
            (lon.lat, lon.lon),
            (nodes.last.vertex.asInstanceOf[LatLon].lat,
              nodes.last.vertex.asInstanceOf[LatLon].lon))).asInstanceOf[Object]))
      case _ =>
    }
    buf.toList
  }
}

class Graph {
  // representing nodes as a list is way too slow, need constant access of hash map
  val nodes = new collection.mutable.HashMap[Vertex, Node]()

  def getNode(v: Vertex): Node = nodes(v)

  def addNode(n: Node): Option[Node] = nodes.put(n.vertex, n)

  override def toString: String = getNodes.mkString("\n")

  def getNodes: List[Node] = nodes.values.toList

  def getVertices: List[Vertex] = nodes.keys.toList

  def totalCost(nodeList: List[Node]): Double = {
    val costs = new collection.mutable.HashMap[(Vertex, Vertex), Double]()
    getEdges.foreach(e => costs.put((e.start, e.end), e.getCost))
    val vertices = nodeList.map(a => a.vertex)
    val nodeListEdges = vertices.slice(0, vertices.length - 1).zip(
      vertices.slice(1, vertices.length)).map(x => costs.get((x._1, x._2)))
    nodeListEdges.flatten.sum
  }

  def getEdges: List[Edge] = nodes.values.toList.map(n => n.getEdges).flatten

  def setCostForEdge(startVertex: Vertex, endVertex: Vertex, cost: Double): Unit = {
    val n = find(endVertex)
    if (n.isEmpty) return
    val nodesToSet = getNeighbors(n.get)
    nodesToSet.foreach(n => n.getEdges.foreach(e => if (e.end == endVertex && e.start == startVertex) e.setCost(cost)))
  }

  def getNeighbors(n: Node): List[Node] = n.getAdjacents.map(v => find(v).get).toList

  def find(v: Vertex): Option[Node] = nodes.get(v)

  def getEdgeCost(startVertex: Vertex, endVertex: Vertex): Double = {
    val n = find(endVertex)
    if (n.isEmpty) return Integer.MAX_VALUE.toDouble
    val nodes = getNeighbors(n.get)
    nodes.foreach(n => n.getEdges.foreach(e => if (e.end == endVertex && e.start == startVertex) return e.getCost))
    Integer.MAX_VALUE.toDouble
  }
}






