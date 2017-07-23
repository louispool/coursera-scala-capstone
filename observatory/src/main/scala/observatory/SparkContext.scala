package observatory

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.log4j.{Level, Logger}

object SparkContext {
  Logger.getLogger("org.apache.spark").setLevel(Level.ERROR)

  val conf: SparkConf    = new SparkConf().setMaster("local").setAppName("Observatory")
  val sc  : SparkContext = new SparkContext(conf)
}
