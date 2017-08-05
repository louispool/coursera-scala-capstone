package observatory

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("Quick check for tile generation") {

    val temps = Extraction.locateTemperatures(2017, "/stations_test.csv", "/temp_test.csv")
    val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)

    val image = Interaction.tile(yearlyAvg, Visualization.temp_colors, 0, 0, 0)

    image.output(new File("Interaction.png"))
  }
}
