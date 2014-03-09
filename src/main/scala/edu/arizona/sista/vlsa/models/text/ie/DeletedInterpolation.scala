package edu.arizona.sista.vlsa.models.text.ie

import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.math.{Stats, VectorMath}
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.DocNeighborhood
import edu.arizona.sista.vlsa.utils.NLPUtils
import java.io.File
import scala.Array
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An information extraction model that interpolates the results (of another information
  * extraction model) at multiple n-gram levels to form a single output for some query.
  *
  * The approach follows from the deleted interpolation method of Brants2000.
  *
  * @constructor A deleted interpolation model that operates on an indexed corpus.
  * @param statesDictionaryFile File containing the list of keywords.
  * @param indexDir Directory for the index of the corpus.
  * @param frequencyDir Directory used to output and/or read cached frequency counts.
  * @param cacheHighlightsDir Directory used to output and/or read cached index searches.
  *
  * @author trananh
  */
class DeletedInterpolation(statesDictionaryFile: String, indexDir: String, frequencyDir: String,
                           cacheHighlightsDir: String) {

  /** Set of mental states */
  val mentalStates = new mutable.HashSet[String]()

  /** Maintain a document neighborhood model for getting search highlights. */
  var docModel = DocNeighborhood.createModel(statesDictionaryFile, indexDir)
  docModel.dictionary.getAllTokens().map(_(0)).foreach(s => mentalStates.add(s))

  /** Load precomputed frequency for the search from file.
    * @param searchTerms The search context to load frequency for.
    */
  def loadFrequency(searchTerms: List[(String, Option[Set[String]])]): Long = {
    val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
    val source = scala.io.Source.fromFile(file)
    val line = source.getLines().next()
    source.close()
    line.toLong
  }

  /** Write the frequency to file using JSON.
    * @param searchTerms The search context of the frequency.
    */
  def saveFrequency(searchTerms: List[(String, Option[Set[String]])], freq: Long) {
    val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
    val writer = new java.io.FileWriter(file)
    writer.write(freq + "\n")
    writer.close()
  }

  /** Pre-compute all frequency counts for the specified query, including counts
    * of unigrams, bigrams, trigrams, etc., and cache the frequency counts to files.
    *
    * @param query A search query.
    */
  def precomputeFrequency(query: List[(String, Option[Set[String]])]) {
    // Pre-compute the frequencies for fast processing later
    for (start <- 0 to 1) {
      for (idx <- start until query.size) {
        val searchTerms = query.slice(start, idx + 1)
        val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
        if (!file.exists()) {
          if (searchTerms.size == 1) {
            // Count all occurrences of this term
            val frequency = docModel.countTermPOS(searchTerms.head, cacheHighlightsDir)
            saveFrequency(searchTerms, frequency)
          } else {
            // Count the number of sentences containing all the terms
            val frequency = docModel.countSentencesWithTermsPOS(searchTerms, cacheHighlightsDir)
            saveFrequency(searchTerms, frequency)
          }
        }
      }
    }
  }

  /** Approximate the deleted interpolation parameters based on statistics found in
    * the corpus.  Based on the algorithm by Brants2000.
    *
    * @param queries All search queries to use for estimating parameters.
    * @param numParams The number of parameters to estimate, should equal the number of query terms + 1.
    * @param statesPOS The part-of-speech for the mental state adjectives.
    * @return Calibrated deleted interpolation estimates.
    */
  def estimateParameters(queries: Array[List[(String, Option[Set[String]])]], numParams: Int,
                         statesPOS: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs)): Array[Double] = {
    // Total number of tokens in the index
    val N = docModel.searcher.getSumTotalTermFreq()

    // Parameters to calibrate
    val lambdas = new Array[Double](numParams)

    // For each query compose a new query by including a mental state.
    queries.foreach(query => {
      mentalStates.toArray.sorted.foreach(state => {
        val newQuery = (state, Option(statesPOS)) :: query
        assert(newQuery.size == numParams)

        // Compute the frequency
        precomputeFrequency(newQuery)

        // Compute the estimated probabilities and use them to adjust the parameters according to Branst2000
        val jointFreq = loadFrequency(newQuery)
        if (jointFreq > 0) {

          val scores = new Array[Double](numParams)
          for (i <- 0 until newQuery.size) {
            val num = loadFrequency(newQuery.slice(0, i + 1)) - 1
            val den = (if (i == 0) N else loadFrequency(newQuery.slice(1, i + 1))) - 1
            if (den == 0)
              scores(i) = 0.0
            else
              scores(i) = num.toDouble / den.toDouble
          }

          // Depending on the max of the score, adjust the parameters accordingly
          val maxIdx = scores.zipWithIndex.maxBy(_._1)._2
          lambdas(maxIdx) += jointFreq
        }

      })
    })

    println(lambdas.mkString(", "))
    VectorMath.normalize(lambdas).toArray
  }

  /** Process a single query tuple using the documents-based neighborhood model and return
    * a distribution over mental states associated with the query.  The final distribution
    * is retrieved via linear interpolation of the frequency counts of each n-gram level.
    *
    * @param query List of search terms and their associated POS restrictions
    *              (set to None for no restrictions).
    * @param lambdas Interpolation parameters, calibrated during training.
    * @param statesPOS The part-of-speech for the mental state adjectives.
    *
    * @return Distribution over mental states for the query, as list of (word, score) pairs.
    */
  def processQueryTuple(query: List[(String, Option[Set[String]])], lambdas: Array[Double],
                        statesPOS: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs))
  : List[(String, Double)] = {
    // Total number of tokens in the index
    val totalTokens = docModel.searcher.getSumTotalTermFreq()

    // The estimated conditional probabilities of each mental state, given the search context.
    val probabilities = new mutable.HashMap[String, Double]()

    // Formulate a new query by adding the mental state as the first query parameter.
    mentalStates.foreach(state => {
      val newQuery = (state, Option(statesPOS)) :: query
      assert(newQuery.size == lambdas.size)

      // Pre-compute the frequency
      precomputeFrequency(newQuery)

      // Now compute the estimated conditional probability of P(state | search-query).
      var p = 0.0
      for (i <- 0 until newQuery.size) {
        val numerator = loadFrequency(newQuery.slice(0, i + 1))
        val denominator = if (i == 0) totalTokens else loadFrequency(newQuery.slice(1, i + 1))
        if (denominator > 0)
          p += (lambdas(i) * numerator.toDouble / denominator.toDouble)
      }

      // Store the probabilities
      probabilities.put(state, p)
    })

    Stats.normalizeScores(probabilities.toList.filter(_._2 > 0.0))
  }

}

/** Run the deleted interpolation algorithm using frequency counts from the documents-based
  * neighborhood model to estimate the interpolation (lambda) parameters
  */
object TrainDeletedInterpolation {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val highlightsCacheDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/highlights/doc"
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/frequency/doc"

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/chase-pilot/xml/"
    val annotationSet = (1 to 4).map(annotationDir + "/chase" + "%02d".format(_) + ".xml")

    // Load annotation to create queries
    val queries = new ListBuffer[(String, String)]()
    annotationSet.foreach(annotationFile => {
      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood("")
      wnm.loadDetections(annotationFile)
      queries.appendAll(wnm.formulateQueriesAA())
    })
    val queriesWithPOS = queries.map(q => {
      List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
    }).toArray

    // Create a model
    val model = new DeletedInterpolation(mentalStatesFile, index, frequencyDir, highlightsCacheDir)

    // Estimate lambda parameters for search triplets
    val lambdas = model.estimateParameters(queriesWithPOS, queriesWithPOS(0).size + 1)
    println("Estimated Parameters: ")
    for (i <- 0 until lambdas.size) {
      println("lambda" + i + " = " + lambdas(i))
    }
  }

}

/** Run the documents-based neighborhood model with deleted interpolation. */
object RunDeletedInterpolation {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model and evaluation
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val highlightsCacheDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/highlights/doc"
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/frequency/doc"

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/chase-pilot/xml/"
    val annotationSet = (1 to 4).map(annotationDir + "/chase" + "%02d".format(_) + ".xml")

    /* Lambda parameters for search tuples (trained on the 4 pilot chase videos).
     *    AA: Array(0.0029411764705882353, 0.36764705882352944, 0.6294117647058823)
     */
    val lambdas = Array(0.0029411764705882353, 0.36764705882352944, 0.6294117647058823)

    // Create a model
    val model = new DeletedInterpolation(mentalStatesFile, index, frequencyDir, highlightsCacheDir)

    // Load annotation to create queries
    annotationSet.foreach(annotationFile => {
      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood("")
      wnm.loadDetections(annotationFile)
      val queries = wnm.formulateQueriesAAL().map(q => {
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