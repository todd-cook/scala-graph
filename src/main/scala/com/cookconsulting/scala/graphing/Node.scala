package com.cookconsulting.scala.graphing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

/**
 * Graph Objects
 */

trait Vertex {
  def cost(other: Vertex): Double = 1D
}

case class XY(x: Int, y: Int) extends Vertex {
  /* Taxicab/"Manhattan" distance */
  override def cost(other: Vertex): Double = {
    (scala.math.abs(x - other.asInstanceOf[XY].x) +
      scala.math.abs(y - other.asInstanceOf[XY].y)).asInstanceOf[Double]
  }
}

case class LatLon(lat: Double, lon: Double) extends Vertex {
  override def cost(other: Vertex): Double = {
    Converter.distanceHaversine((lat, lon), (other.asInstanceOf[LatLon].lat, other.asInstanceOf[LatLon].lon))
  }
}

case class Point(name: Int) extends Vertex

case class Name(name: String) extends Vertex

class Edge(val start: Vertex, val end: Vertex, units: String = "latitude/longitude") {
  var cost: Double = 0D

  def setCost(d: Double) {
    cost = d
  }

  def getCost() = cost

  override def toString(): String = "start: " + start + " end: " + end + " cost: " + cost

  override def hashCode = start.hashCode * 41 * end.hashCode

  def canEqual(other: Any) = other.isInstanceOf[Edge]

  override def equals(other: Any) = other match {
    case that: Edge => (that canEqual this) && (this.start == that.start) && (this.end == that.end)
    case _ => false
  }
}

object NodeState extends Enumeration {
  type NodeState = Value
  val UNVISITED, VISITED, VISITING, INITIAL = Value
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
  }

  def getEdges() = edges

  def addAdjacent(v: Vertex) {
    adjacents = adjacents ::: List(v)
  }

  def getAdjacents(): List[Vertex] = adjacents

  private var state = NodeState.UNVISITED

  def getState() = state

  def setState(s: NodeState) {
    state = s
  }

  override def toString(): String = {
    "Node: " + name + " " + vertex.toString + "\n  adjacents:    \n" + adjacents.mkString("\n")
  }

  override def hashCode = vertex.hashCode

  def canEqual(other: Any) = other.isInstanceOf[Node]

  override def equals(other: Any) = other match {
    case that: Node => (that canEqual this) && (this.vertex == that.vertex)
    case _ => false
  }
}
