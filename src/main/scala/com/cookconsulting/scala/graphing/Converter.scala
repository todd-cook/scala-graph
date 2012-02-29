package com.cookconsulting.scala.graphing

/**
 * @author todd
 * @since 9/12/11 10:24 PM
 */

object Converter {
  /**
   * Calculate distance in meters between the two points using the
   * Haversine formula. <BR>
   * <BR>
   * R. W Sinnott, "Virtues of the Haversine" <BR>
   * Sky and Telescope, vol 68, no 2, 1984 <BR>
   * { @link } http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1 <BR>
   * { @link } http://www.moveable-type.co.uk/scripts/LatLong.html <BR>
   * Since the earth radius varies about 6,378 km equatorial and
   * 6,356 polar errors might be up to about 0.11% at the equator
   * and 0.24% at the poles. The radius of the earth is
   * approximated by using R = (6378 - 22sin((lat1+lat2)/2)) km
   *
   * @param lat1 the latitude of point 1 in degrees
   * @param lon1 the longitude of point 1 in degrees
   * @param lat2 the latitude of point 2 in degrees
   * @param lon2 the longitude of point 2 in degrees
   * @return the distance of the points in meters
   */
  def distanceHaversine(latlon: Tuple2[Double, Double],
                        latlon2: Tuple2[Double, Double]) :Double = {
    // Radius could also be 6,371,000 for average
    val lat1 = scala.math.toRadians(latlon._1)
    val lat2 = scala.math.toRadians(latlon2._1)
    val lon1 = scala.math.toRadians(latlon._2)
    val lon2 = scala.math.toRadians(latlon2._2)
    val radius = 6378000 - 22000 * scala.math.sin((lat1 + lat2) / 2);
    val sin_dlat2 = scala.math.sin((lat2 - lat1) / 2);
    val sin_dlon2 = scala.math.sin((lon2 - lon1) / 2);
    val a = sin_dlat2 * sin_dlat2 + scala.math.cos(lat1) *
      scala.math.cos(lat2) * sin_dlon2 * sin_dlon2;
    val c = 2 * scala.math.atan2(scala.math.sqrt(a),
                                 scala.math.sqrt(1 - a));
    radius * c;
  }

  def metersToMiles(meters: Double): Double = meters * 0.000621371192D


  def milesToMeters(miles: Double): Double = miles / 0.000621371192D
}
