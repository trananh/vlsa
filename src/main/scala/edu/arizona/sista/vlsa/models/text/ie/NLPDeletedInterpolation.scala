package edu.arizona.sista.vlsa.models.text.ie

import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.math.{Stats, VectorMath}
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode
import edu.arizona.sista.vlsa.utils.NLPUtils
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An information extraction model that interpolates the results (of another information
  * extraction model) at multiple n-gram levels to form a single output for some query.
  *
  * The approach follows from the deleted interpolation method of Brants2000.
  *
  * This version requires pre-computed frequency counts.
  *
  * @constructor A deleted interpolation model that operates on an indexed corpus.
  * @param statesDictionaryFile File containing the list of keywords.
  * @param indexDir Directory for the index of the corpus.
  * @param frequencyDir Directory used to output and/or read cached frequency counts.
  * @param docCacheHighlightsDir Directory used to output and/or read cached index searches.
  *
  * @author trananh
  */
class NLPDeletedInterpolation(statesDictionaryFile: String, indexDir: String, frequencyDir: String,
                              docCacheHighlightsDir: String)
  extends DeletedInterpolation(statesDictionaryFile: String, indexDir: String, frequencyDir: String,
    docCacheHighlightsDir: String) {


  /** Approximate the deleted interpolation parameters based on statistics found in
    * the corpus.  Based on the algorithm by Brants2000.
    *
    * @param queries All search queries to use for estimating parameters.
    * @param numParams The number of parameters to estimate, should equal the number of query terms + 1.
    * @param statesPOS The part-of-speech for the mental state adjectives.
    * @return Calibrated deleted interpolation estimates.
    */
  override def estimateParameters(queries: Array[List[(String, Option[Set[String]])]], numParams: Int,
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
  override def processQueryTuple(query: List[(String, Option[Set[String]])], lambdas: Array[Double],
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
  * neighborhood model to estimate the interpolation (lambda) parameters.
  *
  * NOTE: This method REQUIRES previously cached frequency counts to run. Please see:
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunComputeTermPOS]]
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunNLPComputeJointFrequency]]
  *
  */
object TrainNLPDeletedInterpolation {

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
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/frequency/nlp"
    val mode = ActorMode.All

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + act + "-pilot/xml/"
    val annotationSet = (1 to 5).map(annotationDir + act + "%02d".format(_) + ".xml")

    // Load annotation to create queries
    val queries = new ListBuffer[(String, String)]()
    annotationSet.foreach(annotationFile => {

      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood(act, "")

      // Load detections (either for all, only the subject, or only the object).
      mode match {
        case ActorMode.Subject => wnm.loadSubjectDetections(annotationFile)
        case ActorMode.Object => wnm.loadObjectDetections(annotationFile)
        case ActorMode.All => wnm.loadDetections(annotationFile)
        case _ => throw new Exception("Unrecognized mode.")
      }

      // Formulate queries
      queries.appendAll(wnm.formulateQueriesAA())

    })
    val queriesWithPOS = queries.map(q => {
      List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
    }).toArray

    // Create a model
    val model = new NLPDeletedInterpolation(mentalStatesFile, index, frequencyDir, highlightsCacheDir)

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
  * NOTE: This method REQUIRES previously cached frequency counts to run. Please see:
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunComputeTermPOS]]
  *   - [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.RunNLPComputeJointFrequency]]
  *
  */
object RunNLPDeletedInterpolation {

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
    val frequencyDir = "/home/trananh/Workspace/data/vlsa/neighborhood/" + act + "/frequency/nlp"
    val mode = ActorMode.All

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + act + "-pilot/xml/"
    val annotationSet = (1 to 4).map(annotationDir + act + "%02d".format(_) + ".xml")

    // Lambdas for chase-nlp
    val lambdas = Array(0.0035087719298245615, 0.1523809523809524, 0.844110275689223)

    // Create a model
    val model = new NLPDeletedInterpolation(mentalStatesFile, index, frequencyDir, highlightsCacheDir)

    // Load annotation to create queries
    annotationSet.foreach(annotationFile => {

      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood(act, "")

      // Evaluation object
      var eval: Evaluation = null

      // Load detections & evaluation (either for all, only the subject, or only the object).
      mode match {
        case ActorMode.Subject => {
          wnm.loadSubjectDetections(annotationFile)
          eval = new Evaluation(annotationFile, roles = Set(Roles.Subject))
        }

        case ActorMode.Object => {
          wnm.loadObjectDetections(annotationFile)
          eval = new Evaluation(annotationFile, roles = Set(Roles.Object))
        }

        case ActorMode.All => {
          wnm.loadDetections(annotationFile)
          eval = new Evaluation(annotationFile)
        }

        case _ => throw new Exception("Unrecognized mode.")
      }

      // Formulate queries
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

      // Evaluation
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
