package com.cookconsulting.scala.graphing

import collection.mutable.{HashMap, ListBuffer}

/**
 * @author todd
 * @since 2/26/12 2:53 PM
 */
//
//
//class Graph {
//  var nodes = new ListBuffer[Node]()
//
//  def getNodes(): List[Node] = {
//    nodes.toList
//  }
//
//  def addNode(n: Node) = {
//    nodes.append(n)
//  }
//
//  override def toString(): String = getNodes.mkString("\n")
//
//  def find(v: Vertex): Option[Node] = nodes.find(n => n.vertex == v) //.x == v.x && n.vertex.y == v.y)
//
//  def getVertices(): List[Vertex] =
//    getNodes().map(n => n.vertex) ////new Vertex(n.vertex.x, n.vertex.y))
//
//  def getNeighbors(n: Node): List[Node] = {
//    var vertices = n.getAdjacents()
//    vertices.map(v => find(v).get).toList
//  }
//
//  def getEdges(): List[Edge] = {
//    (nodes.toList.map(n => n.getEdges)).flatten
//  }
//
//  def totalCost(nodeList: List[Node]): Double = {
//    val edges = getEdges()
//    val costs = new HashMap[Edge, Double]()
//    edges.foreach(e => costs.put(e, e.getCost))
//    val vertices = nodeList.map(a => a.vertex)
//    val nodeListEdges = ((vertices.slice(0, vertices.length - 1)).zip(
//      vertices.slice(1, vertices.length))).map(x => new Edge(x._1, x._2))
//    nodeListEdges.map(nle => costs.get(nle)).flatten.sum
//  }
//}
