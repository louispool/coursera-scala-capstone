package observatory

import observatory.scheduler.task
import Visualization._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {

    //Grid of temperatures
    val temps = new Array[Double](360*180)

    def calcTemp(lat: Int, lon: Int) = {
      temps(toIdx(lat, lon)) = predictTemperature(temperatures, Location(lat, lon))
    }

    //Calculate the temps for the Grid in parallel
    val tasks = for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180 by 1
    } yield task(calcTemp(lat, lon))

    //Wait for tasks to finish
    tasks.foreach(t => t.join())

    //Return function
    def ret(lat: Int, lon: Int): Double = {
      temps(toIdx(lat, lon))
    }
    ret
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {

    //Grid of averaged temperatures
    val averages = new Array[Double](360 * 180)

    //Sum grid of temperatures for every year
    temperaturess.foreach(temperatures => {

      val grid = makeGrid(temperatures)
      for (lat <- 90 until -90 by -1;
           lon <- -180 until 180 by 1) {

         val idx = toIdx(lat, lon)
         averages(idx) = averages(idx) + grid(lat, lon)
      }
    })

    //Average the temperatures
    val numYears = temperaturess.size
    var idx = 0
    while (idx < averages.length) {
      averages(idx) = averages(idx) / numYears
      idx += 1
    }

    //Return function
    def ret(lat: Int, lon: Int): Double = {
      averages(toIdx(lat, lon))
    }
    ret
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {

    //Grid of temperature deviations
    val deviations = new Array[Double](360 * 180)

    val grid = makeGrid(temperatures)
    for (lat <- 90 until -90 by -1;
         lon <- -180 until 180 by 1) {

      deviations(toIdx(lat, lon)) = grid(lat, lon) - normals(lat, lon)
    }

    //Return function
    def ret(lat: Int, lon: Int): Double = {
      deviations(toIdx(lat, lon))
    }
    ret
  }
}

