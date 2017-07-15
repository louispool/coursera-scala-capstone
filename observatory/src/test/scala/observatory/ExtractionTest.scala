package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("Simple extraction check") {
    val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    temps.foreach(println)

    val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)
    yearlyAvg.foreach(println)
  }
  
}