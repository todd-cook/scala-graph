package com.cookconsulting.scala.graphing

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.collection.mutable.ListBuffer
import org.junit.Assert.assertEquals
import java.io.File


/**
 * @author todd
 * @since 9/12/11 10:22 PM
 */

class GraphBuildingTest extends AssertionsForJUnit {

  @Test
  def testBuildGraph() {

    var myGraph = new Graph()
    myGraph.addNode(new Node(new LatLon(35.25, -119.3), "Bakersfield"))
    myGraph.addNode(new Node(new LatLon(34.51, -116.47), "Barstow"))
    myGraph.addNode(new Node(new LatLon(33.37, -114.43), "Blythe"))
    myGraph.addNode(new Node(new LatLon(34.12, -118.21), "Burbank"))
    myGraph.addNode(new Node(new LatLon(39.48, -121.51), "Chico"))
    myGraph.addNode(new Node(new LatLon(37.58, -121.59), "Concord"))
    myGraph.addNode(new Node(new LatLon(34.5, -117.52), "Covina"))
    buildNeighbors(myGraph, 300)
    //  print(myGraph.getNodes.mkString("\n"))
  }

  /**
   * NOTE, TODO
   * A DMS value is converted to decimal degrees using the formula (D + M/60 + S/3600).
   */
  def loadMapData(pathNFilename: String): List[CityMapPoint] = {
    var mainNode = xml.XML.loadFile(pathNFilename).toList
    var manyNodes = mainNode \\ "cityMapPoint"
    var myCities: List[CityMapPoint] = List[CityMapPoint]()
    var cmp = new CityMapPoint("test", "test", 1D, 2D) // we create a default object to use fromXml
    for (myNode <- manyNodes) {
      myCities = cmp.fromXML(myNode) :: myCities
    }
    myCities.reverse
  }

  def buildNeighbors(graph: Graph, distanceBetween: Double): Graph = {
    val vertices = graph.getVertices()

    /**
     * populate each node of the graph with adjacents according to our heuristic
     */
    graph.getNodes().foreach(n => {
      // don't allow self inclusion
      var verts = vertices.filterNot(a => a == n) //.vertex.x && a.y == n.vertex.y)
      verts.foreach(b => {
        var dist =0D
         if (vertices(0).isInstanceOf[LatLon]){
         dist = Converter.metersToMiles(Converter.distanceHaversine((b.asInstanceOf[LatLon].lat , b.asInstanceOf[LatLon].lon ),
                 (n.vertex.asInstanceOf[LatLon].lat, n.vertex.asInstanceOf[LatLon].lon )))
         } // else add more types
        if (dist < distanceBetween) {
          var myEdge = new Edge(n.vertex, b)
          myEdge.setCost(dist)
          n.addEdge(myEdge)
          //print ("found reasonable neighbor");
          n.addAdjacent(b);
        }
      })
    }
    )
    graph
  }

  def createGraph (distanceBetweenFillups :Int ) :Graph={

    val locations = new File(".").getCanonicalPath + File.separator + "src" + File.separator +
      "main" + File.separator + "resources" + File.separator + "LatLonCityTestData.xml"
    val cities = loadMapData(locations)
    val nodes = cities.map(a => new Node(new LatLon(a.latitude, a.longitude), a.city, a.state))
    var myGraph = new Graph()
    nodes.foreach(n => myGraph.addNode(n))
      myGraph = buildNeighbors(myGraph, distanceBetweenFillups)
            myGraph
    }

  @Test
  def testGraph() {
    val distanceBetweenFillups = 300
    val myGraph = createGraph (distanceBetweenFillups)
    val astar = new AStarImpl(myGraph)
    val losAngeles = myGraph.find(new LatLon(33.56, -118.24)).get
    val newYorkCentralPark = myGraph.find(new LatLon(40.47, -73.58)).get
    val path = astar.compute(losAngeles, newYorkCentralPark).reverse
      val route =  new Route (path)
    println(route)
    println("LA to NY: ")
    println("Maximum distance between fillups: " + distanceBetweenFillups)
       println("Total number of paths considered: " + astar.getNumberOfPaths)
    println("number of getMinDistances: " + astar.getMinDistancesSize)
                   assertEquals (route.totalDistance,2644.25, 0.01 )
  }

    @Test
    def testGraph2(){
    val distanceBetweenFillups = 145
    val myGraph = createGraph (distanceBetweenFillups)

    val astar = new AStarImpl(myGraph)
    val losAngeles = myGraph.find(new LatLon(33.56, -118.24)).get
    val newYorkCentralPark = myGraph.find(new LatLon(40.47, -73.58)).get
    val path = astar.compute(losAngeles, newYorkCentralPark).reverse
    println("LA to NY: ")
    println("Maximum distance between fillups: " + distanceBetweenFillups)
        val route =  new Route (path)
      println(route)
    println("Total number of paths considered: " + astar.getNumberOfPaths)
    println("number of getMinDistances: " + astar.getMinDistancesSize)
   assertEquals (route.totalDistance,2644.75, 0.01 )
 }
}

 /**
   LA to NY:
Maximum distance between fillups: 300
Los Angeles AP (S)	to	Kingman AP	(259.37 miles)
Kingman AP	to	Winslow AP	(206.68 miles)
Winslow AP	to	Los Alamos	(241.14 miles)
Los Alamos	to	Plainview	(287.29 miles)
Plainview	to	Seminole	(293.86 miles)
Seminole	to	Sedalia Whiteman AFB	(283.47 miles)
Sedalia Whiteman AFB	to	Bloomington	(284.55 miles)
Bloomington	to	Marion	(287.80 miles)
Marion	to	State College (S)	(293.25 miles)
State College (S)	to	NYC-Central Park (S)	(206.83 miles)
Total path mileage: 2644.25
Distance as the crow flies: 2480.23
Total number of paths considered: 680
number of getMinDistances: 10
LA to NY:
Maximum distance between fillups: 145
Los Angeles AP (S)	to	Barstow AP	(120.64 miles)
Barstow AP	to	Kingman AP	(140.76 miles)
Kingman AP	to	Prescott AP	(115.89 miles)
Prescott AP	to	Winslow AP	(114.28 miles)
Winslow AP	to	Gallup	(112.06 miles)
Gallup	to	Los Alamos	(129.08 miles)
Los Alamos	to	Raton AP	(123.55 miles)
Raton AP	to	Lajunta AP	(138.98 miles)
Lajunta AP	to	Goodland AP	(119.45 miles)
Goodland AP	to	McCook	(83.13 miles)
McCook	to	Grand Island AP	(119.65 miles)
Grand Island AP	to	Omaha AP	(144.13 miles)
Omaha AP	to	Newton	(122.36 miles)
Newton	to	Iowa City	(96.83 miles)
Iowa City	to	Beloit	(126.58 miles)
Beloit	to	Holland	(132.83 miles)
Holland	to	Adrian	(141.59 miles)
Adrian	to	Cleveland AP (S)	(135.78 miles)
Cleveland AP (S)	to	Butler	(114.17 miles)
Butler	to	State College (S)	(106.04 miles)
State College (S)	to	Paterson	(137.53 miles)
Paterson	to	NYC-Central Park (S)	(69.43 miles)
Total path mileage: 2644.75
Distance as the crow flies: 2480.23
Total number of paths considered: 383
number of getMinDistances: 22
     */