package edu.arizona.sista.vlsa.models.text.ie

import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.CorefNeighborhood
import edu.arizona.sista.vlsa.utils.NLPUtils
import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An information extraction model that interpolates the results (of another information
  * extraction model) at multiple n-gram levels to form a single output for some query.
  *
  * The approach follows from the deleted interpolation method of Brants2000.
  *
  * This version works on extended-highlights, that is highlights that span multiple
  * sentences.
  *
  * @constructor A deleted interpolation model that operates on an indexed corpus.
  * @param statesDictionaryFile File containing the list of keywords.
  * @param indexDir Directory for the index of the corpus.
  * @param frequencyDir Directory used to output and/or read cached frequency counts.
  * @param docCacheHighlightsDir Directory used to output and/or read cached index searches.
  * @param nlpCacheFile Cached file used to store extended nlp highlights.
  *
  * @author trananh
  */
class HighlightsDeletedInterpolation(statesDictionaryFile: String, indexDir: String, frequencyDir: String,
                                     docCacheHighlightsDir: String, nlpCacheFile: File)
  extends DeletedInterpolation(statesDictionaryFile: String, indexDir: String, frequencyDir: String,
                               docCacheHighlightsDir: String) {

  /** Maintain an NLP neighborhood model */
  var nlpModel = CorefNeighborhood.createModel(statesDictionaryFile)


  /** Pre-compute all frequency counts for the specified query, including counts
    * of unigrams, bigrams, trigrams, etc., and cache the frequency counts to files.
    *
    * Frequency of unigrams are computed using the DocNeighborhood model, while
    * frequency of n-grams where (n >= 2) are done using the NLPNeighborhood model.
    *
    * @param query A search query.
    */
  override def precomputeFrequency(query: List[(String, Option[Set[String]])]) {
    // Pre-compute the frequencies for fast processing later
    for (start <- 0 to 1) {
      for (idx <- start until query.size) {
        val searchTerms = query.slice(start, idx + 1)

        val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
        if (!file.exists()) {
          if (searchTerms.size == 1) {
            // Count all occurrences of this term
            val frequency = docModel.countTermPOS(searchTerms.head, docCacheHighlightsDir)
            saveFrequency(searchTerms, frequency)
          } else {
            // Count the number of extended-highlights containing all the terms
            val frequency = nlpModel.countHighlightsWithTermsPOS(searchTerms, nlpCacheFile)
            saveFrequency(searchTerms, frequency)
          }
        }

      }
    }
  }

}

/** Run the deleted interpolation algorithm using frequency counts from the documents-based
  * neighborhood model to estimate the interpolation (lambda) parameters.
  *
  * NOTE: This method depends on previously cached frequency counts to run efficiently.
  * It is capable of computing the frequency as needed, but this process is much slower.
  *
  * It is STRONGLY recommended to batch pre-compute the frequency counts by first, see:
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunComputeTermPOS]]
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunCorefComputeJointFrequency]]
  *
  */
object TrainHighlightsDeletedInterpolation {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model
    val act = "hug"
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val highlightsCacheDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/highlights/doc"
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/frequency/nlp-coref"
    val nlpCachedFile = new File("/Volumes/MyPassport/data/vlsa/neighborhood/" + act +
      "/highlights/nlp/coref.txt")

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + act + "-pilot/xml/"
    val annotationSet = (1 to 5).map(annotationDir + act + "%02d".format(_) + ".xml")

    // Load annotation to create queries
    val queries = new ListBuffer[(String, String)]()
    annotationSet.foreach(annotationFile => {
      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood(act, "")
      wnm.loadDetections(annotationFile)
      queries.appendAll(wnm.formulateQueriesAA())
    })
    val queriesWithPOS = queries.map(q => {
      List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
    }).toArray

    // Create a model
    val model = new HighlightsDeletedInterpolation(mentalStatesFile, index, frequencyDir, highlightsCacheDir, nlpCachedFile)

    // Estimate lambda parameters for search triplets
    val lambdas = model.estimateParameters(queriesWithPOS, queriesWithPOS(0).size + 1)
    println("Estimated Parameters: ")
    for (i <- 0 until lambdas.size) {
      println("lambda" + i + " = " + lambdas(i))
    }
  }

}

/** Run the documents-based neighborhood model with deleted interpolation.
  *
  * NOTE: This method depends on previously cached frequency counts to run efficiently.
  * It is capable of computing the frequency as needed, but this process is much slower.
  *
  * It is STRONGLY recommended to batch pre-compute the frequency counts by first, see:
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunComputeTermPOS]]
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunCorefComputeJointFrequency]]
  *
  */
object RunHighlightsDeletedInterpolation {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model and evaluation
    val act = "chase"
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val highlightsCacheDir = "/home/trananh/Workspace/data/vlsa/neighborhood/" + act + "/highlights/doc"
    val frequencyDir = "/home/trananh/Workspace/data/vlsa/neighborhood/" + act + "/frequency/nlp-coref"
    val nlpCachedFile = new File("/home/trananh/Workspace/data/vlsa/neighborhood/" + act +
      "/highlights/nlp/coref.txt")

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + act + "-pilot/xml/"
    val annotationSet = (1 to 4).map(annotationDir + act + "%02d".format(_) + ".xml")

    // Lambda parameters for chase-coref
    val lambdas = Array(0.0, 0.08650236657417985, 0.9134976334258201)

    // Create a model
    val model = new HighlightsDeletedInterpolation(mentalStatesFile, index, frequencyDir,
      highlightsCacheDir, nlpCachedFile)

    // Load annotation to create queries
    annotationSet.foreach(annotationFile => {
      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood(act, "")
      wnm.loadDetections(annotationFile)
      val queries = wnm.formulateQueriesAA().map(q => {
        List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
      }).toArray

      // Run query and average the distribution of each triplet
      val allResults = new mutable.HashMap[String, Double]()
      queries.foreach(query => {
        val result = model.processQueryTuple(query, lambdas)
        result.foreach(kv => {
          allResults.put(kv._1, allResults.getOrElse(kv._1, 0.0) + kv._2)
        })
      })
      val distribution = allResults.toList.map(kv => (kv._1, kv._2 / queries.size))
      assert(math.abs(distribution.map(_._2).sum - 1.0) < Stats.DoubleEpsilon)

      // Evaluation object
      val eval = new Evaluation(annotationFile)
      println("\n====\nEvaluation for file " + annotationFile + "\n")
      println("\n====\nFinal results:")
      println(distribution.sortWith(_._2 > _._2).mkString(", "))
      eval.F1(distribution.map(_._1).toSet)
      eval.SAF1(distribution.map(_._1))
      eval.CWSAF1(distribution)
      NeighborhoodExperiment.estimateCWSAF1Params(distribution.toList, eval)

    })
  }

}
