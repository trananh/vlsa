package edu.arizona.sista.vlsa.experiments

import java.io.{File, FileWriter}

import edu.arizona.sista.vlsa.models.data.VideoAnnotation
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.evaluation.Evaluation.MeasureType
import edu.arizona.sista.vlsa.models.evaluation.Evaluation.MeasureType.MeasureType

import scala.collection.mutable.{HashMap, ListBuffer}

/** A metric evaluation experiment.
  *
  * @param distributionsDir Directory containing sample distribution text files.
  */
class MetricEvaluationExperiment(val distributionsDir: String) {

  // Distributions keyed by name IDs
  val distributions = new HashMap[String, List[(String, Double)]]()
  val distributionTempFiles = new HashMap[String, File]()

  // Load distributions
  loadDistributions(distributionsDir)
  createTempAnnotationFiles(distributions)


  /** Load distributions from text files using the filenames as IDs.
    *
    * Example file format: R0.txt
    *
    * (angry, 0.9); (afraid, 0.05); (guilty, 0.05)
    *
    * @param dirPath Path to directory containing distribution text files.
    */
  private def loadDistributions(dirPath: String) {
    val dir = new File(dirPath)
    dir.listFiles().filter(f => f.isFile && f.getName.endsWith(".txt")).foreach(f => {
      val id = f.getName.stripSuffix(".txt")
      val source = scala.io.Source.fromFile(f)
      val line = source.getLines().next()
      source.close()
      val distribution = new ListBuffer[(String, Double)]()
      line.split(";").foreach(token => {
        val parts = token.trim().replace("(", "").replace(")", "").split(",")
        assert(parts.size == 2)
        distribution.append((parts(0).trim, parts(1).trim.toDouble))
      })

      distributions.put(id, distribution.toList.sortWith((_._2 > _._2)))
    })
  }

  /** Create a temp annotation file containing the distribution.
    * @param fileID File ID used to identify the distribution.
    * @param distribution The distribution to write to file.
    * @return A temp annotation file containing the distribution.
    */
  private def writeTempFile(fileID: String, distribution: List[(String, Double)]): File = {
    val annotation = new VideoAnnotation(fileID + ".xml", "activity")
    distribution.foreach(item => {
      annotation.addMentalStates(VideoAnnotation.Roles.Subject, List.fill((item._2 * 100).toInt)(item._1) :_*)
    })
    val groundTruthDataFile = java.io.File.createTempFile("tmp", ".xml")
    val writer = new FileWriter(groundTruthDataFile)
    writer.write(annotation.toXML())
    writer.close()
    groundTruthDataFile.deleteOnExit()
    groundTruthDataFile
  }

  /** Create temp annotation files for all loaded distributions.
    * @param distributions The distributions.
    */
  private def createTempAnnotationFiles(distributions: HashMap[String, List[(String, Double)]]) {
    distributions.foreach(kv => {
      distributionTempFiles.put(kv._1, writeTempFile(kv._1, kv._2))
    })
  }

  /** Load test configurations from CSV config file.
    * @param testIndex CSV File containing test configurations.
    */
  private def loadTestConfigurations(testIndex: String): List[(String, String, String)] = {
    val testConfigs = new ListBuffer[(String, String, String)]()
    val source = scala.io.Source.fromFile(new File(testIndex))
    val lines = source.getLines
    val headers = lines.next().split(",")
    val targetIdx = headers.indexOf("Target")
    val optionAIdx = headers.indexOf("OptionA")
    val optionBIdx = headers.indexOf("OptionB")
    lines.foreach(l => {
      val tokens = l.split(",")
      testConfigs.append((tokens(targetIdx).trim, tokens(optionAIdx).trim, tokens(optionBIdx).trim))
    })
    testConfigs.toList
  }


  /** Evaluate the test cases given by the CSV index file.
    * The CSV index file must have a header line with the following 3 columns: Target, OptionA, OptionB
    *
    * Example file format: test.csv
    *
    * Target,OptionA,OptionB
    * R0,R1,R2
    * R0,R1,R3
    *
    * @param testIndex CSV File containing test configurations.
    * @param measure Type of measure to use.
    */
  def evaluateCWSAF1(testIndex: String, measure: MeasureType = MeasureType.CWSAF1) {
    println("Target,OptionA,OptionB,Selection,Score")
    val testConfigs = loadTestConfigurations(testIndex)
    testConfigs.foreach(test => {
      val eval = new Evaluation(distributionTempFiles.get(test._1).get.getAbsolutePath)
      eval.log = false

      var optionA = 0.0
      var optionB = 0.0

      measure match {

        case MeasureType.Vec => {
          // Use cosine similarity
          eval.loadVectors("/path/to/vectors.bin")
          optionA = eval.vecDistributionalSimilarity(distributions.get(test._2).get)._2
          optionB = eval.vecDistributionalSimilarity(distributions.get(test._3).get)._2
        }

        case MeasureType.F1 => {
          optionA = eval.F1(distributions.get(test._2).get.map(_._1).toSet)._1
          optionB = eval.F1(distributions.get(test._3).get.map(_._1).toSet)._1
        }

        case MeasureType.WSAF1 => {
          optionA = eval.WSAF1(distributions.get(test._2).get)._1
          optionB = eval.WSAF1(distributions.get(test._3).get)._1
        }

        /* Defaults to CWSA-F1 */
        case _ => {
          optionA = eval.CWSAF1(distributions.get(test._2).get)._1
          optionB = eval.CWSAF1(distributions.get(test._3).get)._1
        }
      }

      print(test._1 + "," + test._2 + "," + test._3 + ",")
      if (optionA > optionB) println(test._2 + "," + (optionA - optionB).toString)
      else println(test._3 + "," + (optionB - optionA).toString)
    })
  }

}

/** Evaluate a performance measure */
object RunMetricEvaluationExperiment {

  def main(args: Array[String]) {
    val distributionsDir = "/Users/trananh/Dropbox/Documents/Research/Dissertation/experiments/MTurk/similarity/distributions"
    val testIndex = "/Users/trananh/Dropbox/Documents/Research/Dissertation/experiments/MTurk/similarity/input.csv"

    val cwsaEval = new MetricEvaluationExperiment(distributionsDir)
    cwsaEval.evaluateCWSAF1(testIndex, MeasureType.CWSAF1)
  }

}
