package observatory


import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("Visualization check") {

    var t0 = System.nanoTime()
    val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val yearlyAvg = Extraction.locationYearlyAverageRecords(temps)

    var timeElapsed = (System.nanoTime() - t0) / 1e9
    println(f"Time taken for Temperature Extraction: $timeElapsed%.0fs")

    t0 = System.nanoTime()
    val image = Visualization.visualize(yearlyAvg, Seq[(Double, Color)]((60, Color(255, 255, 255)),
                                                                        (32, Color(255, 0, 0)),
                                                                        (12, Color(255, 255, 0)),
                                                                        (0, Color(0, 255, 255)),
                                                                        (-15, Color(0, 0, 255)),
                                                                        (-27, Color(255, 0, 255)),
                                                                        (-50, Color(33, 0, 107)),
                                                                        (-60, Color(0, 0, 0))))

    timeElapsed = (System.nanoTime() - t0) / 1e9
    println(f"Time take for Visualization: $timeElapsed%.0fs")

    image.output(new File("Visualization.png"))
  }

  test("Interpolation") {

    val color = Visualization.interpolateColor(Seq[(Double, Color)]((60, Color(255, 255, 255)),
                                                                    (32, Color(255, 0, 0)),
                                                                    (12, Color(255, 255, 0)),
                                                                    (0, Color(0, 255, 255)),
                                                                    (-15, Color(0, 0, 255)),
                                                                    (-27, Color(255, 0, 255)),
                                                                    (-50, Color(33, 0, 107)),
                                                                    (-60, Color(0, 0, 0))), 0)
    println(f"${color.red}, ${color.blue}, ${color.green}")
  }

}
