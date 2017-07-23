package observatory

import java.io.File
import java.time.LocalDate

import observatory.SparkContext._

case class Station(key: String, location: Location)
case class Temp(stationKey: String, date: LocalDate, temp: Double)

/**
 * 1st milestone: data extraction
 */
object Extraction {

  private def filePath(path: String) = new File(getClass.getClassLoader.getResource(if (path.startsWith("/")) path.substring(1) else path).toURI).getPath

  private def fahrenheitToCelsius(F: Double) = (F - 32.0) * (5.0/9.0)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsRDD = sc.textFile(filePath(stationsFile)).map(line => line.split(",", -1))
                                                         .filter(cols => !(cols(2).isEmpty || cols(3).isEmpty))
                                                         .map(cols => {
                                                           val key = cols(0) + ":" + cols(1)
                                                           (key, Station(key, Location(cols(2).toDouble, cols(3).toDouble)))
                                                         })

    val tempRDD = sc.textFile(filePath(temperaturesFile)).map(line => line.split(",", -1))
                                                         .map(cols => {
                                                           val key = cols(0) + ":" + cols(1)
                                                           (key, Temp(key, LocalDate.of(year, cols(2).toInt, cols(3).toInt), fahrenheitToCelsius(cols(4).toDouble)))
                                                         })

    stationsRDD.join(tempRDD).mapValues(pair => (pair._2.date, pair._1.location, pair._2.temp)).values.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    sc.parallelize(records.toSeq).map(triple => (triple._2, triple._3))
                                 .aggregateByKey((0.0, 0))((acc, temp) => (acc._1 + temp, acc._2 + 1),
                                                           (acc1, acc2) =>  (acc1._1 + acc2._1, acc1._2 + acc2._2))
                                 .mapValues(sum => sum._1 / sum._2).collect()
  }
}
