package com.cookconsulting.scala.graphing

import collection.mutable.ListBuffer

/**
 * @author todd
 * @since 2/26/12 2:52 PM
 */


class Route(val nodes: List[Node]) {
  def start = nodes(0)

  def end = nodes(nodes.length - 1)

  val distances = createDistances()
  val totalDistance = distances.sum

  def createDistances() = {
    if (nodes(0).vertex.isInstanceOf[LatLon]) {
      val vertices = nodes.map(a => (a.vertex.asInstanceOf[LatLon].lat, a.vertex.asInstanceOf[LatLon].lon))
      ((vertices.slice(1, vertices.length)).zip(vertices.slice(0, vertices.length - 1))).map(
        a => Converter.metersToMiles(Converter.distanceHaversine((a._1._1, a._1._2), (a._2._1, a._2._2))))
    }
    else {
      List()
    }
  }

  override def toString() = getRouteList().mkString("\n")

  def getRouteList() = {
    val buf = new ListBuffer[String]()
    Iterator.range(0, distances.length).foreach(i => {
      buf.append(nodes(i).name + "\tto\t" + nodes(i + 1).name + "\t"
                   + "(" + String.format("%.2f", distances(i).asInstanceOf[Object]) + " miles)")
    })
    buf.append("Total path mileage: " + java.lang.String.format("%.2f", distances.sum.asInstanceOf[Object]))
    if (nodes(0).vertex.isInstanceOf[LatLon]) {
      buf.append("Distance as the crow flies: " + String.format("%.2f",
                  Converter.metersToMiles(Converter.distanceHaversine(
                    (nodes(0).vertex.asInstanceOf[LatLon].lat, nodes(0).vertex.asInstanceOf[LatLon].lon),
                    (nodes(nodes.length - 1).vertex.asInstanceOf[LatLon].lat,
                      nodes(nodes.length - 1).vertex.asInstanceOf[LatLon].lon))).asInstanceOf[Object]))
    }
    buf.toList
  }
}
