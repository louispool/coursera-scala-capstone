package observatory

import java.io.File
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Manipulation.average
import observatory.Visualization.interpolateColor
import observatory.scheduler.parallel

import scala.collection.mutable.ArrayBuffer

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  //Color Scale
  val dev_colors: Seq[(Double, Color)] = Seq[(Double, Color)]((7, Color(0, 0, 0)),
                                                              (4, Color(255, 0, 0)),
                                                              (2, Color(255, 255, 0)),
                                                              (0, Color(255, 255, 255)),
                                                              (-2, Color(0, 255, 255)),
                                                              (-7, Color(0, 0, 255)))

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(x: Double, y: Double,
                            d00: Double, d01: Double,
                            d10: Double, d11: Double): Double = d00*(1 - x)*(1 - y) + d10*x*(1 - y) + d01*(1 - x)*y + d11*x*y

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(grid: (Int, Int) => Double, colors: Iterable[(Double, Color)],
                    zoom: Int, x: Int, y: Int): Image = {

    //Image buffer
    val pixels = new Array[Pixel](256 * 256)

    def calcTile(zoom: Int, depth: Int, x: Int, y: Int, row: Int, col: Int, pixels: Array[Pixel]): Unit = {

      if (depth == 8) {

        val loc = tileLocation(zoom, x, y)
        val lon0 = Math.floor(loc.lon).toInt
        val lat0 = Math.ceil(loc.lat).toInt

        val temp = bilinearInterpolation(loc.lon - lon0, lat0 - loc.lat,
                                         grid(lat0, lon0), grid(lat0 - 1, lon0),
                                         grid(lat0, lon0 + 1), grid(lat0 - 1, lon0 + 1))
        pixels(row*256 + col) = {
          val color = interpolateColor(colors, temp)
          Pixel(color.red, color.green, color.blue, 127)
        }
      } else {

        parallel(calcTile(zoom + 1, depth + 1, 2*x, 2*y, row*2, col*2, pixels), //Top Left
                 calcTile(zoom + 1, depth + 1, 2*x + 1, 2*y, row*2, col*2 + 1, pixels), //Top Right
                 calcTile(zoom + 1, depth + 1, 2*x, 2*y + 1, row*2 + 1, col*2, pixels), //Bottom Left
                 calcTile(zoom + 1, depth + 1, 2*x + 1, 2*y + 1, row*2 + 1, col*2 + 1, pixels)) // Bottom Right
      }
    }

    //Calculate the pixels
    calcTile(zoom, 0, x, y, 0, 0, pixels)

    //Return finished image
    Image(256, 256, pixels)
  }

  def normals(startYr: Int, endYr: Int): (Int, Int) => Double = {

    val temperaturess = new ArrayBuffer[Iterable[(Location, Double)]](endYr - startYr + 1)
    for (year <- startYr to endYr) {
      val temps = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
      temperaturess += Extraction.locationYearlyAverageRecords(temps)
    }
    average(temperaturess)
  }

  def deviations(startYr: Int, endYr: Int, normals: (Int, Int) => Double): Iterable[(Int, (Int, Int) => Double)] = {

    val deviations = new ArrayBuffer[(Int, (Int, Int) => Double)](endYr - startYr + 1)
    for (year <- startYr to endYr) {
      val temps = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
      val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)

      deviations += Tuple2(year, Manipulation.deviation(yearlyAvg, normals))
    }
    deviations
  }

  def main(args: Array[String]): Unit = {

//    var t0 = System.nanoTime()
    val normals = Visualization2.normals(1989, 1989)
//    var timeElapsed = (System.nanoTime() - t0) / 1e9
//    println(f"Time taken for computation of Normals: $timeElapsed%.0fs")

//    t0 = System.nanoTime()
    val deviations = Visualization2.deviations(2015, 2015, normals)
//    timeElapsed = (System.nanoTime() - t0) / 1e9
//    println(f"Time taken for computation of Deviations: $timeElapsed%.0fs")

    def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: (Int, Int) => Double): Unit = {

      val image = Visualization2.visualizeGrid(data, Visualization.temp_colors, zoom, x, y)

      val dir = s"target/deviations/$year/$zoom"
      Files.createDirectories(Paths.get(dir))

      image.output(new File(s"$dir/$x-$y.png"))
    }
    Interaction.generateTiles(deviations, generateImage)
  }
}
