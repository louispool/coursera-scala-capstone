package observatory

import java.io.File
import java.lang.Math._
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.scheduler._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {

    val n = (1 << zoom).toDouble; //pow(2, zoom)

    val lat = toDegrees(atan(sinh(PI*(1.0 - 2.0*y / n))))
    val lon = x / n * 360.0 - 180.0

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {

    //Image buffer
    val pixels = new Array[Pixel](256*256)

    def calcTile(zoom: Int, depth: Int, x: Int, y: Int, row: Int, col: Int, pixels: Array[Pixel]): Unit = {
      if (depth == 8) {
        pixels(row*256 + col) = Visualization.calcPixel(temperatures, colors, tileLocation(zoom, x, y), 127)
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

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Int, Data)],
                          generateImage: (Int, Int, Int, Int, Data) => Unit): Unit = {

    def zoomIn(year: Int, zoom: Int, x: Int, y: Int, data: Data): Unit = {

      generateImage(year, zoom, x, y, data)
      if (zoom < 3){
        parallel(zoomIn(year, zoom + 1, 2*x, 2*y, data), //Top Left
                 zoomIn(year, zoom + 1, 2*x + 1, 2*y, data), //Top Right
                 zoomIn(year, zoom + 1, 2*x, 2*y + 1, data), //Bottom Left
                 zoomIn(year, zoom + 1, 2*x + 1, 2*y + 1, data)) // Bottom Right
      }
    }

    for ((year, data) <- yearlyData) {
      zoomIn(year, 0, 0, 0, data)
    }
  }

  def main(args: Array[String]): Unit = {

    def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]): Unit = {

      val image = Interaction.tile(data, Visualization.temp_colors, zoom, x, y)

      val dir = s"target/temperatures/$year/$zoom"
      Files.createDirectories(Paths.get(dir))

      image.output(new File(s"$dir/$x-$y.png"))
    }

    //Generate tiles for 2015
    val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)

    Interaction.generateTiles(Seq[(Int, Iterable[(Location, Double)])]((2015, yearlyAvg)), generateImage)
  }
}
