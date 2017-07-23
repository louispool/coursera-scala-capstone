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

    val colors = Seq[(Double, Color)]((60, Color(255, 255, 255)),
                                      (32, Color(255, 0, 0)),
                                      (12, Color(255, 255, 0)),
                                      (0, Color(0, 255, 255)),
                                      (-15, Color(0, 0, 255)),
                                      (-27, Color(255, 0, 255)),
                                      (-50, Color(33, 0, 107)),
                                      (-60, Color(0, 0, 0)))

    var t0 = System.nanoTime()
    val temps = Extraction.locateTemperatures(2017, "/stations_test.csv", "/temp_test.csv")
    val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)

    var timeElapsed = (System.nanoTime() - t0) / 1e9
    println(f"Time taken for Temperature Extraction: $timeElapsed%.0fs")

    t0 = System.nanoTime()
    val image = Interaction.tile(yearlyAvg, colors, 0, 0, 0)

    timeElapsed = (System.nanoTime() - t0) / 1e9
    println(f"Time take for Interaction: $timeElapsed%.0fs")

    image.output(new File("Interaction.png"))
  }
}
