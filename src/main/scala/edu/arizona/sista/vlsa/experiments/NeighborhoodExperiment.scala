package edu.arizona.sista.vlsa.experiments

import java.io.File

import edu.arizona.sista.vlsa.math.{Stats, VectorMath}
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.{Baseline, DocNeighborhood, VecNeighborhood, WebNeighborhood}
import edu.arizona.sista.vlsa.search.lucene.IndexSearcher
import edu.arizona.sista.vlsa.utils.NLPUtils

import scala.collection.mutable.ListBuffer

/** Provides methods to run different neighborhood models and estimate hyper-parameters.
  *
  * @author trananh
  */
object NeighborhoodExperiment {

  /** Seek the best filter parameters that maximize the F1 score for the given distribution.
    *
    * @param response The response distribution.
    * @param eval Evaluation data.
    */
  def estimateF1Params(response: List[(String, Double)], eval: Evaluation) = {
    var massTotal = 0.0
    val mass = new ListBuffer[Double]()
    val sorted = response.sortWith(_._2 > _._2)
    val F1s = new ListBuffer[(Double, Double, Double)]()

    // F1 scores
    eval.log = false
    val slice = new ListBuffer[String]()
    for (i <- 0 until sorted.size) {
      slice.append(sorted(i)._1)
      F1s.append(eval.F1(slice.toSet))
      massTotal += sorted(i)._2
      mass.append(massTotal)
    }
    println("\nSet F1 (" + sorted.size + ")")
    eval.log = true

    println(sorted.mkString(", "))
    val zippedF1sReversed = F1s.zipWithIndex.reverse.filterNot(_._1._1.isNaN)
    if (zippedF1sReversed.size > 0) {
      var i = zippedF1sReversed.maxBy(_._1._1)._2
      println("Best F1: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._2)._2
      println("Best P: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._3)._2
      println("Best R: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))

      println("f = [" + F1s.map(_._1).mkString(", ") + "]")
      println("p = [" + F1s.map(_._2).mkString(", ") + "]")
      println("r = [" + F1s.map(_._3).mkString(", ") + "]")
      println("mass = [" + mass.mkString(", ") + "]")
    }
  }

  /** Seek the best filter parameters that maximize the SA-F1 score for the given distribution.
    *
    * @param response The response distribution.
    * @param eval Evaluation data.
    */
  def estimateSAF1Params(response: List[(String, Double)], eval: Evaluation) = {
    var massTotal = 0.0
    val mass = new ListBuffer[Double]()
    val sorted = response.sortWith(_._2 > _._2)
    val gold = eval.goldSet.toList.sorted
    val F1s = new ListBuffer[(Double, Double, Double)]()

    // Compute all pair-wise sim scores
    val scores = eval.calcSimilarityMatrix(sorted.map(_._1), gold)

    // Similarity F1 scores
    eval.log = false
    val slice = new ListBuffer[String]()
    for (i <- 0 until sorted.size) {
      // Slice of top-scoring elements
      slice.append(sorted(i)._1)

      // Accumulate total mass for the slice
      massTotal += sorted(i)._2
      mass.append(massTotal)

      // Find best matching pairs between gold & response
      val responseMaxMatches = VectorMath.maxColumnIdx(scores, rowRange = (0, i+1)).map(_(0))
      val goldMaxMatches = VectorMath.maxRowIdx(scores, rowRange = (0, i+1)).map(_(0))

      // Find F1s
      F1s.append(eval.calcSAF1(slice.toList, gold, responseMaxMatches, goldMaxMatches, scores))
    }
    println("\nSimilarity-Aligned F1 (" + sorted.size + ")")
    eval.log = true

    println(sorted.mkString(", "))
    val zippedF1sReversed = F1s.zipWithIndex.reverse.filterNot(_._1._1.isNaN)
    if (zippedF1sReversed.size > 0) {
      var i = zippedF1sReversed.maxBy(_._1._1)._2
      println("Best F1: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._2)._2
      println("Best P: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._3)._2
      println("Best R: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))

      println("f = [" + F1s.map(_._1).mkString(", ") + "]")
      println("p = [" + F1s.map(_._2).mkString(", ") + "]")
      println("r = [" + F1s.map(_._3).mkString(", ") + "]")
      println("mass = [" + mass.mkString(", ") + "]")
    }
  }

  /** Seek the best filter parameters that maximize the WSA-F1 score for the given distribution.
    *
    * @param response The response distribution.
    * @param eval Evaluation data.
    */
  def estimateWSAF1Params(response: List[(String, Double)], eval: Evaluation) = {
    var massTotal = 0.0
    val mass = new ListBuffer[Double]()
    val sorted = response.sortWith(_._2 > _._2)
    val gold = eval.goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._2 > _._2)
    val F1s = new ListBuffer[(Double, Double, Double)]()

    // Compute all pair-wise sim scores
    val scores = eval.calcSimilarityMatrix(sorted.map(_._1), gold.map(_._1))

    // Similarity F1 scores
    eval.log = false
    val slice = new ListBuffer[(String, Double)]()
    var sliceNorm = slice.toList
    for (i <- 0 until sorted.size) {
      // Slice of top-scoring elements
      slice.append(sorted(i))

      // Accumulate total mass for the slice
      massTotal += sorted(i)._2
      mass.append(massTotal)

      // Redistribute the distribution in the slice
      sliceNorm = slice.map(e => (e._1, e._2 / massTotal)).toList

      // Find best matching pairs between gold & response
      val responseMaxMatches = VectorMath.maxColumnIdx(scores, rowRange = (0, i+1)).map(_(0))
      val goldMaxMatches = VectorMath.maxRowIdx(scores, rowRange = (0, i+1)).map(_(0))

      // Find F1s
      F1s.append(eval.calcWSAF1(sliceNorm, gold, responseMaxMatches, goldMaxMatches, scores))
    }
    println("\nWeighted Similarity-Aligned F1 (" + sorted.size + ")")
    eval.log = true

    // We reverse so that we can get the first instance of max-by
    println(sorted.mkString(", "))
    val zippedF1sReversed = F1s.zipWithIndex.reverse.filterNot(_._1._1.isNaN)
    if (zippedF1sReversed.size > 0) {
      val j = zippedF1sReversed.maxBy(_._1._1)._2
      println("Best F1: " + (F1s(j), j) + ", " + mass(j) + ", " + (sorted(j)._2/sorted(0)._2))
      var i = zippedF1sReversed.maxBy(_._1._2)._2
      println("Best P: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._3)._2
      println("Best R: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))

      println("f = [" + F1s.map(_._1).mkString(", ") + "]")
      println("p = [" + F1s.map(_._2).mkString(", ") + "]")
      println("r = [" + F1s.map(_._3).mkString(", ") + "]")
      println("mass = [" + mass.mkString(", ") + "]")

      // Once we found the best F1, re-do the calculation and print out more log for it
      println("=> Distribution Weights (WSA) for BEST F1")
      sliceNorm = sorted.slice(0, j+1).map(e => (e._1, e._2 / mass(j))).toList
      val responseMaxMatches = VectorMath.maxColumnIdx(scores, rowRange = (0, j+1)).map(_(0))
      val goldMaxMatches = VectorMath.maxRowIdx(scores, rowRange = (0, j+1)).map(_(0))
      eval.calcWSAF1(sliceNorm, gold, responseMaxMatches, goldMaxMatches, scores)
    }
  }

  /** Seek the best filter parameters that maximize the CWSA-F1 score for the given distribution.
    *
    * @param response The response distribution.
    * @param eval Evaluation data.
    */
  def estimateCWSAF1Params(response: List[(String, Double)], eval: Evaluation) = {
    var massTotal = 0.0
    val mass = new ListBuffer[Double]()
    val sorted = response.sortWith(_._2 > _._2)
    val gold = eval.goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._2 > _._2)
    val F1s = new ListBuffer[(Double, Double, Double)]()

    // Compute all pair-wise sim scores
    val scores = eval.calcSimilarityMatrix(sorted.map(_._1), gold.map(_._1))

    // Similarity F1 scores
    eval.log = false
    val slice = new ListBuffer[(String, Double)]()
    var sliceNorm = slice.toList
    for (i <- 0 until sorted.size) {
      // Slice of top-scoring elements
      slice.append(sorted(i))

      // Accumulate total mass for the slice
      massTotal += sorted(i)._2
      mass.append(massTotal)

      // Redistribute the distribution in the slice
      sliceNorm = slice.map(e => (e._1, e._2 / massTotal)).toList

      // Find best matching pairs between gold & response
      val responseMaxMatches = VectorMath.maxColumnIdx(scores, rowRange = (0, i+1))
      val goldMaxMatches = VectorMath.maxRowIdx(scores, rowRange = (0, i+1))

      // Find F1s
      F1s.append(eval.calcCWSAF1(sliceNorm, gold, responseMaxMatches, goldMaxMatches, scores))
    }
    println("\nConstrained Weighted Similarity F1 (" + sorted.size + ")")
    eval.log = true

    // We reverse so that we can get the first instance of max-by
    println(sorted.mkString(", "))
    val zippedF1sReversed = F1s.zipWithIndex.reverse.filterNot(_._1._1.isNaN)
    if (zippedF1sReversed.size > 0) {
      val j = zippedF1sReversed.maxBy(_._1._1)._2
      println("Best F1: " + (F1s(j), j) + ", " + mass(j) + ", " + (sorted(j)._2/sorted(0)._2))
      var i = zippedF1sReversed.maxBy(_._1._2)._2
      println("Best P: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))
      i = zippedF1sReversed.maxBy(_._1._3)._2
      println("Best R: " + (F1s(i), i) + ", " + mass(i) + ", " + (sorted(i)._2/sorted(0)._2))

      println("f = [" + F1s.map(_._1).mkString(", ") + "]")
      println("p = [" + F1s.map(_._2).mkString(", ") + "]")
      println("r = [" + F1s.map(_._3).mkString(", ") + "]")
      println("mass = [" + mass.mkString(", ") + "]")

      // Once we found the best F1, re-do the calculation and print out more log for it
      println("=> BEST F1")
      sliceNorm = sorted.slice(0, j+1).map(e => (e._1, e._2 / mass(j))).toList
      val responseMaxMatches = VectorMath.maxColumnIdx(scores, rowRange = (0, j+1))
      val goldMaxMatches = VectorMath.maxRowIdx(scores, rowRange = (0, j+1))
      eval.calcCWSAF1(sliceNorm, gold, responseMaxMatches, goldMaxMatches, scores)
    }

  }


  /** Run a documents-based neighborhood model and return the model.
    *
    * @param statesDictionary Dictionary file containing list of mental states.
    * @param indexDir Root directory of the index.
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param targetPOSLabels Set of POS labels used to filter the tokens
    *                        (default to adjective + verb labels).
    * @param dataDir Path to directory containing the cached highlights from a previous search.
    *                Caching search results can speed up computation significantly.  Setting
    *                this value to None (default) will trigger a new index search.
    *
    * @return A documents-based neighborhood model.
    */
  def runDocNeighborhood(statesDictionary: String, indexDir: String,
                         searchTerms: List[(String, Option[Set[String]])],
                         targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                         dataDir: Option[String] = None)
  : DocNeighborhood = {
    val model = DocNeighborhood.createModel(statesDictionary, indexDir)
    model.processSearchTermsWithPOS(searchTerms, IndexSearcher.DEFAULT_MAX_HITS, targetPOSLabels, cacheDir = dataDir)
    model
  }

  /** Run a web-based neighborhood model and return the model.
    *
    * @param statesDictionary Dictionary file containing list of mental states.
    * @param dataDir Path to directory containing search results pulled from the web.
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param targetPOSLabels Set of POS labels used to filter the tokens
    *                        (default to adjective + verb labels).
    *
    * @return A web-based neighborhood model.
    */
  def runWebNeighborhood(statesDictionary: String, dataDir: String,
                         searchTerms: List[(String, Option[Set[String]])],
                         targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs))
  : WebNeighborhood = {
    val model = WebNeighborhood.createModel(statesDictionary)
    model.searchTermsWithPOS(searchTerms, dataDir, targetPOSLabels)
    model
  }

  /** Run a vectors-based neighborhood model and return the model.
    *
    * @param statesDictionary Dictionary file containing list of mental states.
    * @param vecFile Binary file of the trained word2vec model.
    * @param searchTerms List of search terms.
    *
    * @return A web-based neighborhood model.
    */
  def runVecNeighborhood(statesDictionary: String, vecFile: String, searchTerms: List[String])
  : VecNeighborhood = {
    val model = VecNeighborhood.createModel(statesDictionary, vecFile)
    model.process(searchTerms)
    model.vecModel.get.clear()
    model
  }


  /** Evaluate a neighborhood information extraction model using the given ground-truth data.
    *
    * @param model A neighborhood information extraction model.
    * @param groundTruthFile File containing ground-truth mental states.
    * @param vecFile Binary file for a trained word2vec model.
    */
  def evaluate(model: Any, groundTruthFile: String, vecFile: String) {
    // Start new evaluation against the specified ground-truth data
    val eval = new Evaluation(groundTruthFile)

    // Get scores and convert to probs (0 scores are excluded)
    var scores: List[(String, Double)] = null
    model match {
      case model: DocNeighborhood => {
        scores = model.getRankedResults(DocNeighborhood.Scoring.RelevanceRate)
      } case model: WebNeighborhood => {
        scores = model.getRankedResults(WebNeighborhood.Scoring.Score)
      } case model: VecNeighborhood => {
        scores = model.getRankedResults(VecNeighborhood.Scoring.ExpEuclideanBased)
      } case model: Baseline => {
        scores = model.getRankedResults()
      }
    }
    val probs = ListBuffer(scores.map(_._1).zip(VectorMath.normalize(scores.map(_._2))).toSeq: _*)

    // KL-Divergence (must fill in words in the gold that's not in the output)
    val klProbs = probs.clone()
    val probsKeys = klProbs.map(_._1).toSet
    eval.goldSet.foreach(w => {
      // add words in the gold that's not in the output with a tiny positive value
      if (!probsKeys.contains(w))
        klProbs.append((w, Stats.SmallestPositiveValue))
    })
    eval.KL(klProbs.toSet)

    // Vectors space distributional distance
    eval.loadVectors(vecFile)
    eval.vecDistributionalSimilarity(probs.toList)
    eval.clearVectors()

    // Grab top results
    val results = probs.sortWith(_._2 > _._2)

    // F1
    eval.F1(results.toList.map(_._1).toSet)

    // SA F1
    eval.SAF1(results.toList.map(_._1))

    // WSA F1
    eval.WSAF1(results.toList, uniformExperiment = true)

    // CWSA F1
    eval.CWSAF1(results.toList, uniformExperiment = true)
    estimateCWSAF1Params(results.toList, eval)

  }


  /** Run various neighborhood models. */
  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Ground-truth directory and mental state dictionary
    val groundTruthDir = "/Volumes/MyPassport/data/annotations/chase-pilot/xml/"
    val statesDictionary = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val vecFile = "/Volumes/MyPassport/data/text/entity-vectors/Gigaword-story/story-vecs.bin"

    val evalSet = (1 to 4).map("chase" + "%02d".format(_) + ".xml")

    // The search context (query tuple)
    val searchContext = List(("chase", Option(NLPUtils.POS_VBs)))

    // Run document neighborhood experiment
    val docDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/highlights/doc"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val docModel = runDocNeighborhood(statesDictionary, index, searchContext, dataDir = Option(docDataDir))
    println("\nEVALUATING DOC NEIGHBORHOOD\n")
    evalSet.foreach(file => {
      println("\n====\nEvaluating against " + file)
      evaluate(docModel, groundTruthDir + file, vecFile)
    })
    docModel.clear()

    // Run web neighborhood experiment
    val webDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/bing"
    val webModel = runWebNeighborhood(statesDictionary, webDataDir, searchContext)
    println("\nEVALUATING WEB NEIGHBORHOOD\n")
    evalSet.foreach(file => {
      println("\n====\nEvaluating against " + file)
      evaluate(webModel, groundTruthDir + file, vecFile)
    })
    webModel.clear()

    // Run vector neighborhood experiment
    val vecModel = runVecNeighborhood(statesDictionary, vecFile, searchContext.map(_._1))
    println("\nEVALUATING VEC NEIGHBORHOOD\n")
    evalSet.foreach(file => {
      println("\n====\nEvaluating against " + file)
      evaluate(vecModel, groundTruthDir + file, vecFile)
    })
    vecModel.clear()

    // Run uniform baseline model experiment
    val baseModel = new Baseline()
    baseModel.loadAdjectiveDictionary(new File(statesDictionary), defaultScore = 1.0)
    println("\nEVALUATING BASELINE MODEL\n")
    evalSet.foreach(file => {
      println("\n====\nEvaluating against " + file)
      evaluate(baseModel, groundTruthDir + file, vecFile)
    })
    baseModel.clear()
  }

}
