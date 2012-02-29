package com.cookconsulting.scala.graphing

/**
 * Used for loading the XML
 *
 * @author todd
 * @since 9/18/11 10:22 AM
 */

class CityMapPoint(val state: String, val city: String,
                   val latitude: Double, val longitude: Double) extends Ordered[CityMapPoint] {

  require(state != null && city != null)

  private var dist: Double = 0 // temp placeholder
  def distance(): Double = dist

  def distance_(x: Double) = {
    dist = x
  }

  def setDistance(x: Double) = {
    dist = x
  }

  def compare(that: CityMapPoint): Int = {
    var result: Int = -1
    if (this.distance == that.distance) {
      result = 0
    }
    if (this.distance < that.distance) {
      result = -1
    }
    if (this.distance > that.distance) {
      result = 1
    }
    result
  }

  private var neighborCities: List[CityMapPoint] = List[CityMapPoint]()

  def neighbors = neighborCities

  def addNeighbor(city: CityMapPoint): Unit = {
    neighborCities = city :: neighborCities
  }

  def setNeighbors(hood: List[CityMapPoint]) = {
    neighborCities = hood
  }

  def toXml =
        <cityMapPoint>
        <state>{state}</state>
        <city>{city}</city>
        <latLon>{latitude}, {longitude}</latLon>
        </cityMapPoint>

  def fromXML(node: scala.xml.Node): CityMapPoint = {
    //TODO match case
    // 34ÔøΩ09?22?N 118ÔøΩ7?55?W  when format is determined
    //  var latLon = (node \ "latLon")
    // and convert using: A DMS value is converted to decimal degrees using the formula (D + M/60 + S/3600).
    new CityMapPoint((node \ "state").text, (node \ "city").text,
                     (new java.lang.Double((node \ "latLon").text.split(",")(0).trim())).asInstanceOf[Double],
                     (new java.lang.Double((node \ "latLon").text.split(",")(1).trim())).asInstanceOf[Double])
  }

  override def toString = state + " " + city + " (" + latitude.toString + ", " + longitude.toString + ")"

  override def hashCode = {
    var result = state.hashCode
    result = 31 * result + city.hashCode
    result = 31 * result + latitude.hashCode
    result = 31 * result + longitude.hashCode
    result
  }

  override def equals(other: Any) = other match {
    case that: CityMapPoint =>
      (that canEqual this) &&
        (this.state == that.state) &&
        (this.city == that.city) &&
        (this.latitude == that.latitude) &&
        (this.longitude == that.longitude)
    case _ =>
      false
  }

  def canEqual(other: Any) = other.isInstanceOf[CityMapPoint]
}
