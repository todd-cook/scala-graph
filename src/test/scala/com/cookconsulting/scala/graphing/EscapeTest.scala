package com.cookconsulting.scala.graphing;

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import scala.math._

/**
 * From Top Coder Algorithm tutorials:
 * (please visit their site and sign up: http://topcoder.com )
 * Problem Statement for Escape
 * You are playing a video game that involves escaping from a dangerous area.
 * Within the area there are DEADLY regions you can't enter, HARMFUL regions 
 * that take 1 life for every step you make in them, and NORMAL regions that
 * don't affect you in any way. You will start from (0,0) and have to make it 
 * to (500,500) using only Up, Left, Right, and Down steps. The map will be 
 * given as a String[] deadly listing the DEADLY regions and a String[] harmful
 * listing the HARMFUL regions. The elements in each of these parameters will
 * be formatted as follows:
 * 
 * Input format(quotes for clarity): "X1 Y1 X2 Y2" where
 * (X1,Y1) is one corner of the region and
 * (X2,Y2) is the other corner of the region
 * 
 * The corners of the region are inclusive bounds (i.e. (4,1) and (2,2) 
 * include x-values between 4 and 2 inclusive and y-values between 1 and 
 * 2 inclusive). All unspecified regions are considered NORMAL. If regions 
 * overlap for a particular square, then whichever region is worst takes 
 * effect (e.g. DEADLY+HARMFUL = DEADLY, HARMFUL+NORMAL = HARMFUL, 
 * HARMFUL+HARMFUL = HARMFUL, DEADLY+NORMAL=DEADLY).
 * Damage taken at each step occurs based on the destination square and not 
 * on the starting square (e.g. if the square (500,500) is HARMFUL you WILL 
 * take a point of damage stepping onto it; if the square (0,0) is HARMFUL 
 * you WON'T take a point of damage stepping off of it; this works 
 * analogously for DEADLY squares).
 * Return the least amount of life you will have to lose in order to reach 
 * the destination. Return -1 if there is no path to the destination. Your
 * character is not allowed to leave the map (i.e. have X or Y less than 0
 *  or greater than 500).
 * Notes
 *  -	If two harmful regions overlap, the area where they overlap is exactly
 *  the same as non-overlapping harmful regions (i.e. the effect is NOT 
 *  cumulative, and the overlapping region still takes exactly 1 life)
 *  Constraints
 * -	Each element of deadly and harmful will be of the form (quotes for
 *      clarity): "X1 Y1 X2 Y2"
 *      where X1,Y1,X2, and Y2 are integers between 0 and 500 inclusive
 *      and contain no leading zeros
 * -	Each element of deadly and harfmul will contain no leading, 
 *      trailing or extra whitespace
 *  
 * Examples given in the unit test below
 * @author : ToddCook
 * @since : 2/27/12 11:03 AM
 */
class EscapeTest extends AssertionsForJUnit {

    def createBasicGraph( rows:Int, cols:Int) :Graph={
        val minX = 0
        val minY = 0
        val maxX = rows + 1
        val maxY = cols + 1
        var graph = new Graph ()
        Iterator.range(0, maxX).foreach(x => {
            Iterator.range(0, maxY).foreach(y => {
            var n = new Node (new XY (x, y))
            if (x-1 >= minX)  {
                n.addEdge (new Edge (new XY (x, y), new XY(x-1, y)))
                        n.addAdjacent(new XY (x-1, y))
                        }
            if (y-1 >= minY)  {
                n.addEdge(new Edge (new XY (x, y), new XY(x, y-1)))
                        n.addAdjacent(new XY (x, y-1))
                        }
            if (x+1 <  maxX)   {
                n.addEdge(new Edge (new XY (x, y), new XY(x+1, y)))
                        n.addAdjacent(new XY (x+1, y))
                }
                if (y+1 <  maxY)    {
                n.addEdge(new Edge (new XY (x, y), new XY(x, y+1)))
                        n.addAdjacent(new XY (x, y+1))
            }
            graph.addNode(n)
            })
        })
     graph
    }

    def generateEdges(costAR:Array[Array[Double]],
                      minXY:Tuple2[Int,Int], maxXY:Tuple2[Int,Int],
                      graph :Graph) ={
        // walk the double array pushing setCost (val1, val2, min(val1,val2) )
        // set the cost of the edge based on the destination coordinate value
        val minX = minXY._1
        val minY = minXY._2
        val maxX = maxXY._1 + 1
        val maxY = maxXY._2 + 1
        Iterator.range(0, maxX).foreach(x => {
            Iterator.range(0, maxY).foreach(y => {
            if (x-1 >= minX)  {
                graph.setCostForEdge(new XY(x, y), new XY(x - 1, y),costAR(x - 1)(y))
                        }
            if (y-1 >= minY)  {
                graph.setCostForEdge( new XY (x, y), new XY(x, y - 1),
                    costAR(x)(y - 1) )
                        }
            if (x+1 <  maxX)   {
                graph.setCostForEdge( new XY (x, y), new XY(x + 1, y),
                    costAR(x + 1)(y) )
                }
                if (y+1 <  maxY)    {
                graph.setCostForEdge( new XY (x, y), new XY(x, y + 1),
                    costAR(x)(y + 1))
            }
            })
        })
     graph
    }

    
    def buildCostGrid( costAR:Array[Array[Double]], coords :Tuple2[Int,Int], coords2:Tuple2[Int,Int],
                 cost:Double) :Array[Array[Double]] ={
        val minX = min(coords._1, coords2._1)
        val maxX = max(coords._1, coords2._1)
        val minY = min(coords._2, coords2._2)
        val maxY = max(coords._2, coords2._2)
        // push value regions into a double array 
        // then walk the double array pushing setCost (val1, val2, min(val1,val2) ) 
        Iterator.range(maxY, minY - 1, -1).foreach (y=>{
        Iterator.range(minX, maxX + 1).foreach (x=>{costAR(x)(y) = cost})})
     costAR
    }

    def lowest (harmful :List[String], deadly :List[String]) :Double = {
        val rows = 500
        val cols = 500
        var graph = createBasicGraph (rows,cols)
        var costAR = Array.fill(rows+1,cols+1)(1d)
          //generate cost Array    
        harmful.foreach(line => { var coords = line.split(" ").map(_.toInt).toList
         costAR = buildCostGrid( costAR, (coords(0), coords(1)), (coords(2), coords(3)), 2d)
        } )

        deadly.foreach(line => { var coords = line.split(" ").map(_.toInt).toList
         costAR = buildCostGrid(costAR, (coords(0), coords(1)), (coords(2), coords(3)),
          Integer.MAX_VALUE.toDouble)
        } )
        graph = generateEdges(costAR, (0,0), (rows, cols), graph)
        var start = graph.find(new XY (0,0)).get
        var end = graph.find(new XY (rows,cols)).get
        //var vertex1 = new XY(251,250)
        //var vertex2 = new XY(250,250)
        //println (vertex1 + " to " + vertex2 +" " + graph.getEdgeCost(vertex1, vertex2))
        //println(graph.getNode(vertex1)) 
        //println("start: " + start)
        //println("end: " + end)
        var astar = new AStarImpl (graph)
        var path = astar.compute(end, start ) // , end)
        //var shortestPathVertices =  path.map( n => n.vertex)
        //println("shortest path: "+ shortestPathVertices)
        var totalCost = graph.totalCost (path)
        println("total cost: " + totalCost)
        var absCost = (totalCost - Converter.taxicabDistance( (0,0), ( rows, cols)))
        println("absolute cost: "+ absCost )
        if (absCost < 0d || absCost >= Integer.MAX_VALUE.toDouble) return -1d
	absCost
    }
    
    @Test
    def test2(){
    println("running test 2")
        assertEquals(1000, (lowest ( List("0 0 250 250", "250 250 500 500"),
                List("0 251 249 500", "251 0 500 249"))).toInt )
    }

    @Test
    def checkCosts(){
      val rows = 5
        val cols = 5
        var graph = createBasicGraph (rows,cols)
        graph.setCostForEdge( new XY(3,3),new XY(2,3), 2d)
      //  graph.getEdges.foreach(e => println(e))
    }

    @Test
    def test0()  {
        println("running test 0")
      assertEquals (0, (lowest(List[String](), List[String]())).toInt )
    }

    @Test
    def test1()  {
    // (0,0) is DEADLY but that doesn't affect our path since we never
    // step onto it (only from it). The rest of the map is NORMAL.
        println("running test 1")
      assertEquals (1000, (lowest(List("500 0 0 500"), List("0 0 0 0"))).toInt )
    }

    @Test
    def test3(){
         println("running test 3")
        assertEquals(-1, (lowest(List("0 0 250 250","250 250 500 500"),
                List("0 250 250 500","250 0 500 250"))).toInt )
    }
   
    @Test
    def test4(){
          println("running test 4")
        assertEquals(254, (lowest( List("468 209 456 32",
             "71 260 306 427",
             "420 90 424 492",
             "374 253 54 253",
             "319 334 152 431",
             "38 93 204 84",
             "246 0 434 263",
             "12 18 118 461",
             "215 462 44 317",
             "447 214 28 475",
             "3 89 38 125",
             "157 108 138 264",
             "363 17 333 387",
             "457 362 396 324",
             "95 27 374 175",
             "381 196 265 302",
             "105 255 253 134",
             "0 308 453 55",
             "169 28 313 498",
             "103 247 165 376",
             "264 287 363 407",
             "185 255 110 415",
             "475 126 293 112",
             "285 200 66 484",
             "60 178 461 301",
             "347 352 470 479",
             "433 130 383 370",
             "405 378 117 377",
             "403 324 369 133",
             "12 63 174 309",
             "181 0 356 56",
             "473 380 315 378"),
             List( "250 384 355 234",
             "28 155 470 4",
             "333 405 12 456",
             "329 221 239 215",
             "334 20 429 338",
             "85 42 188 388",
             "219 187 12 111",
             "467 453 358 133",
             "472 172 257 288",
             "412 246 431 86",
             "335 22 448 47",
             "150 14 149 11",
             "224 136 466 328",
             "369 209 184 262",
             "274 488 425 195",
             "55 82 279 253",
             "153 201 65 228",
             "208 230 132 223",
             "369 305 397 267",
             "200 145 98 198",
             "422 67 252 479",
             "231 252 401 190",
             "312 20 0 350",
             "406 72 207 294",
             "488 329 338 326",
             "117 264 497 447",
             "491 341 139 438",
             "40 413 329 290",
             "148 245 53 386",
             "147 70 186 131",
             "300 407 71 183",
             "300 186 251 198",
             "178 67 487 77",
             "98 158 55 433",
             "167 231 253 90",
             "268 406 81 271",
             "312 161 387 153",
             "33 442 25 412",
             "56 69 177 428",
             "5 92 61 247"))).toInt )
    }
}