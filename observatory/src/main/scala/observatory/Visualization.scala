package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.SparkContext._
import observatory.scheduler._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  //Color Scale
  val temp_colors: Seq[(Double, Color)] = Seq[(Double, Color)]((60, Color(255, 255, 255)),
                                                               (32, Color(255, 0, 0)),
                                                               (12, Color(255, 255, 0)),
                                                               (0, Color(0, 255, 255)),
                                                               (-15, Color(0, 0, 255)),
                                                               (-27, Color(255, 0, 255)),
                                                               (-50, Color(33, 0, 107)),
                                                               (-60, Color(0, 0, 0)))
  //IDW: Power Parameter
  private val p = 6

  //Mean radius of the Earth
  private val RADIUS: Double = 6371000.0

  //Haversine formula (numerically better-conditioned for small distances than traditional arc-cosine formula)
  def haversineDistance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = 2*asin(sqrt(pow(sin((lat2 - lat1) / 2.0), 2) +
                                                                                                      cos(lat1)*cos(lat2) * pow(sin((lon2 - lon1) / 2.0), 2)))
  //Distance between Locations
  def distance(loc1: Location, loc2: Location): Double = RADIUS*haversineDistance(toRadians(loc1.lat), toRadians(loc1.lon),
                                                                                  toRadians(loc2.lat), toRadians(loc2.lon))

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature_SPARK(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val distToTempRDD = sc.parallelize(temperatures.toSeq).map(temps => (temps._2, distance(temps._1, location))) //PairRDD[(Temp, Dist)]

    //check for sensor data closer than 1km
    val smallDistRDD = distToTempRDD.filter(_._2 < 1000)
    //return temp of closest location
    if (!smallDistRDD.isEmpty()) {
      smallDistRDD.min()((x: (Double, Double), y: (Double, Double)) => Ordering[Double].compare(x._2, y._2))._1
    } else {
      //IDW: wi(x) = 1 / d(x,xi)^p
      val invDistWeightRDD = distToTempRDD.map(tuple => (tuple._1, 1.0 / pow(tuple._2, p))) //PairRDD[(Temp, InvDist)]

      //IDW: ∑wi*zi / ∑wi
      invDistWeightRDD.map(tuple => tuple._1 * tuple._2).sum() / invDistWeightRDD.values.sum()
    }
  }

  /**
   * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
   * @param location     Location where to predict the temperature
   * @return The predicted temperature at `location`
   */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val distToTemp = temperatures.map(temps => (temps._2, distance(temps._1, location))) //Array[(Temp, Dist)]

    //check for sensor data closer than 1km
    if (distToTemp.exists(_._2 < 1000)) {
      //return temp of closest location
      distToTemp.filter(_._2 < 1000).minBy(_._2)._1
    } else {
      //IDW: wi(x) = 1 / d(x,xi)^p
      val invDistWeight = distToTemp.map(tuple => (tuple._1, 1.0 / pow(tuple._2, p))) //Array[(Temp, InvDist)]

      //IDW: ∑wi*zi / ∑wi
      invDistWeight.map(tuple => tuple._1 * tuple._2).sum / invDistWeight.unzip._2.sum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    def lerp(c1: Color, c2: Color, t: Double) = Color(((c1.red   + (c2.red   - c1.red)*t) + 0.5).toInt,
                                                      ((c1.green + (c2.green - c1.green)*t) + 0.5).toInt,
                                                      ((c1.blue  + (c2.blue  - c1.blue)*t) + 0.5).toInt)

    val sortedPoints = points.toSeq.sortBy(_._1) //Seq[(Double, Color)]

    val idx = sortedPoints.indexWhere(point => point._1 >= value)
    if (idx < 0) {
      sortedPoints.last._2 //Lowest Temperature
    } else if (idx == 0) {
      sortedPoints.head._2 //Highest Temperature
    } else {
      val pt1 = sortedPoints(idx - 1)
      val pt2 = sortedPoints(idx)

      //Interpolate Color
      lerp(pt1._2, pt2._2, (value - pt1._1) / (pt2._1 - pt1._1))
    }
  }

  def calcPixel(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], loc: Location, alpha: Int): Pixel = {

    val temp = predictTemperature(temperatures, loc)
    val color = interpolateColor(colors, temp)

    Pixel(color.red, color.green, color.blue, alpha)
  }

  def toIdx(lat: Int, lon: Int): Int = -360 * (lat - 90) + (lon + 180)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    //Image dimensions
    val imgHeight = 180
    val imgWidth = 360

    val pixels = new Array[Pixel](imgWidth*imgHeight)
    
    def calcTile(startLat: Int, endLat: Int,
                 startLon: Int, endLon: Int) = {

      for (lat <- startLat until endLat by -1;
           lon <- startLon until endLon by  1) {

        pixels(toIdx(lat, lon)) = calcPixel(temperatures, colors, Location(lat, lon), 255)
      }
    }

    //Number of tiles used during visualization on the horizontal and vertical (ie. total number of tiles = numTiles^2)
    //For now needs to divide the image dimensions cleanly with no remainder
    val numTiles = 6

    //Accumulate concurrent tasks
    val rowInc = -imgHeight / numTiles
    val colInc = imgWidth / numTiles

    val tasks = for {
       lat <- 90 until -90 by rowInc
       lon <- -180 until 180 by colInc
    } yield task(calcTile(lat, lat + rowInc,
                          lon, lon + colInc))

    //Wait for tasks to finish
    tasks.foreach(t => t.join())

    //Return finished image
    Image(imgWidth, imgHeight, pixels)
  }
}

