package edu.arizona.sista.vlsa.models.evaluation

import edu.arizona.sista.vlsa.math.{Stats, Metrics, VectorMath}
import edu.arizona.sista.vlsa.models.data.VideoAnnotation
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles.Roles
import edu.arizona.sista.vlsa.models.evaluation.Evaluation.SimilarityScore
import edu.arizona.sista.vlsa.models.evaluation.Evaluation.SimilarityScore.SimilarityScore
import edu.arizona.sista.vlsa.models.text.word.Word2Vec
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import edu.arizona.sista.vlsa.utils.NLPUtils
import edu.cmu.lti.lexical_db.NictWordNet
import edu.cmu.lti.ws4j.impl._
import edu.cmu.lti.ws4j.util.WS4JConfiguration
import scala.Array
import scala.collection.mutable.ListBuffer

/** Evaluation methods.
  *
  * @param groundTruthData Path to the ground-truth XML file.
  *
  * @author trananh
  */
class Evaluation(var groundTruthData: String, var roles: Set[Roles] = Set(Roles.Subject, Roles.Object)) {

  /** Log the scores computed to standard out if true. */
  var log = true

  /** Load ground-truth data */
  val goldFreq = loadGroundTruthLabels(groundTruthData, roles)
  val goldSet = goldFreq.getOccurringTokens().map(_(0))

  /** Load vector binary space */
  val vectors = new Word2Vec()
  var vectorsLoaded = false

  /** Read in the mental-states XML annotation and return a frequency dictionary.
    * Note that the mental states are passed through lemmatization.
    * @param annotationFile The annotation file
    * @param roles Specify which person to extract mental states for (default
    *              to both the subject and the object.
    * @return Frequency dictionary of mental states.
    */
  private def loadGroundTruthLabels(annotationFile: String, roles: Set[Roles])
  : FrequencyDictionary[String] = {
    // Get the mental states
    val video = VideoAnnotation.fromXML(annotationFile)
    val terms = new ListBuffer[String]()
    roles.foreach(role => terms.appendAll(video.getMentalStates(role)))

    // Lemmatize the states (as adjectives)
    val lemmas = NLPUtils.lemmatizeTermsAsAdjectives(terms.toArray)

    // Count states frequency
    val statesFrequency = new FrequencyDictionary[String]()
    statesFrequency.addEntries(lemmas.filter(_.length > 0))

    statesFrequency
  }

  /** Load word2vec vector space from the trained binary file.
    * @param vecBinFile Path to the binary file of the trained word2vec vector space.
    */
  def loadVectors(vecBinFile: String) {
    vectors.load(vecBinFile)
    vectorsLoaded = true
  }

  /** Clears the vectors space from memory. */
  def clearVectors() {
    vectors.clear()
    vectorsLoaded = false
  }


  /** Word similarity calculator(s)
    * - HSO (Hirst & St-Onge, 1998):
    *     Two lexicalized concepts are semantically close if their WordNet synsets are
    *     connected by a path that is not too long and that "does not change direction
    *     too often".
    */
  val db = new NictWordNet()
  WS4JConfiguration.getInstance.setMFS(false)
  val simCalcHSO = new HirstStOnge(db)

  /** Compute the similarity score between two words using a WordNet similarity score calculator.
    * The score is normalized to be within [0.0, 1.0].
    * @param s1 First word.
    * @param s2 Second word.
    * @param simScore The word-similarity calculator to use (defaults to HSO).
    * @return The similarity score between two words.
    */
  private def calcWordNetSim(s1: String, s2: String, simScore: SimilarityScore = SimilarityScore.HSO): Double = {
    var maxScore = 0.0
    simScore match {
      case SimilarityScore.HSO => {
        maxScore = 16.0
        val sort = Array(s1, s2).sorted
        return math.min(simCalcHSO.calcRelatednessOfWords(sort(0), sort(1)), maxScore) / maxScore
      }
    }
    throw new Exception("Unrecognized Similarity Calculator!")
  }

  /** Compute the similarity score between two words using the word2vec vectors space.
    * The score is normalized to be within [0.0, 1.0].
    * @param s1 First word.
    * @param s2 Second word.
    * @return The similarity score between two words.
    */
  private def calcWord2VecCosine(s1: String, s2: String): Double = {
    val sort = Array(s1, s2).sorted
    if (!vectors.contains(sort(0)) || !vectors.contains(sort(0)))
      return (0.0 + 1.0) / 2.0
    vectors.cosine(sort(0), sort(1)) + 1.0 / 2.0
  }

  /** Calculate the similarity score for each pair of elements between the two given lists.
    * This requires looping over each pair, which can be very computationally expensive!
    *
    * The scores are normalized to be within [0.0, 1.0].
    *
    * @param rowList The list to map along the row of the resulting scores matrix.
    * @param colList The list to map along the column of the resulting scores matrix.
    * @param simScore The word-similarity function to use (defaults to HSO).
    *
    * @return 2D matrix containing similarity scores for all pairs between the given lists.
    */
  def calcSimilarityMatrix(rowList: List[String], colList: List[String],
                           simScore: SimilarityScore = SimilarityScore.HSO)
  : Array[Array[Double]] = {
    // Compute pair-wise scores and store results in a 2D matrix.
    val scores = Array.ofDim[Double](rowList.size, colList.size)

    simScore match {

      // WordNet similarity
      case SimilarityScore.HSO => {
        for (i <- 0 until rowList.size) {
          for (j <- 0 until colList.size) {
            scores(i)(j) = calcWordNetSim(rowList(i), colList(j), simScore = simScore)
          }
        }
      }

      // Word2vec similarity
      case SimilarityScore.W2V => {
        if (!vectorsLoaded) throw new Exception("Vectors space not loaded!")
        for (i <- 0 until rowList.size) {
          for (j <- 0 until colList.size) {
            scores(i)(j) = calcWord2VecCosine(rowList(i), colList(j))
          }
        }
      }

    }

    scores
  }
  
  
  /** Compute the KL-divergence from the ground-truth distribution to the given distribution.
    * @param response Probability distribution over states as a set of (state, value) pairs.
    * @return The KL-divergence, denoted as DKL(gold || response).
    */
  def KL(response: Set[(String, Double)]): Double = {
    // Make sure the gold set contains all keys in the response set
    val goldClone = goldFreq.clone()
    response.foreach(o => goldClone.createEntry(o._1))
    val goldDistribution = goldClone.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._1 < _._1)

    // Make sure the response set contains all keys in the gold set
    val responseClone = collection.mutable.Map(response.toSeq: _*)
    goldSet.foreach(g => if (!responseClone.contains(g)) responseClone.put(g, 0.0))
    val responseDistribution = responseClone.toList.sortWith(_._1 < _._1)

    // Compute KL-divergence
    goldDistribution.zip(responseDistribution).foreach(e => assert(e._1._1.equals(e._2._1), "Unmatched distributions!"))
    val kl = Metrics.klDivergence(goldDistribution.map(_._2).toArray, responseDistribution.map(_._2).toArray)
    if (log) println("\nKL-Divergence = " + kl)

    kl
  }

  /** Compute the F1 score from the ground-truth set to the given set.
    * @param response Set to compute the score for.
    * @return Triplet of scores (F1, precision, recall).
    */
  def F1(response: scala.collection.Set[String]): (Double, Double, Double) = {
    // Compute the scores
    val tpSet = goldSet.intersect(response)
    val fpSet = response.diff(goldSet)
    val fnSet = goldSet.diff(response)
    val (tp, fp, fn) = (tpSet.size, fpSet.size, fnSet.size)
    val precision = Metrics.precision(tp.toDouble, fp.toDouble)
    val recall = Metrics.recall(tp.toDouble, fn.toDouble)
    val f1 = Metrics.F1(precision, recall)

    // Print log
    if (log) println("\nTP = " + tp + " " + tpSet.toString)
    if (log) println("FP = " + fp + " " + fpSet.toString)
    if (log) println("FN = " + fn + " " + fnSet.toString)
    if (log) println("Precision = " + precision)
    if (log) println("Recall = " + recall)
    if (log) println("F1 = " + f1)

    (f1, precision, recall)
  }

  /** Compute the Similarity-Aligned (SA) F1 score between a response list and a ground-truth list.
    *
    * @param response The response list of labels to compute score for.
    * @param gold The ground-truth list of labels to compute score against.
    * @param responseMatches Array specifying the index of the "best-matching" gold
    *                        item for each response item.
    * @param goldMatches Array specifying the index of the "best-matching" response item
    *                    for each gold item.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def calcSAF1(response: List[String], gold: List[String],
                responseMatches: Array[Int], goldMatches: Array[Int], scores: Array[Array[Double]])
  : (Double, Double, Double) = {

    // Take care of edge case
    if (response.size == 0) {
      val p = 1.0; val r = 0.0; val f1 = Metrics.F1(p, r)
      return (f1, p, r)
    }

    // Compute similarity aligned precision & recall
    val precision = response.zipWithIndex.map(e => scores(e._2)(responseMatches(e._2))).sum / response.size.toDouble
    val recall = gold.zipWithIndex.map(e => scores(goldMatches(e._2))(e._2)).sum / gold.size.toDouble
    val f1 = Metrics.F1(precision, recall)
    if (log) println("\nSA Precision = " + precision)
    if (log) println("SA Recall = " + recall)
    if (log) println("SA F1 = " + f1)

    (f1, precision, recall)
  }

  /** Compute the Similarity-Aligned (SA) F1 score between a response list and a ground-truth list.
    *
    * @param response The response list of labels to compute score for.
    * @param gold The ground-truth list of labels to compute score against.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *               If an empty array is given, new scores will be computed using the default
    *               word-similarity function.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def SAF1(response: List[String], gold: List[String], scores: Array[Array[Double]]): (Double, Double, Double) = {
    // For each response label, find its best matching gold label (according to similarity score)
    val responseMaxMatches = VectorMath.maxColumnIdx(scores).map(_(0))

    // For each gold label, find its best matching response label (according to similarity score)
    val goldMaxMatches = VectorMath.maxRowIdx(scores).map(_(0))

    // Compute similarity-constraint F1 score
    calcSAF1(response, gold, responseMaxMatches, goldMaxMatches, scores)
  }

  /** Compute the Similarity-Aligned (SA) F1 score between a response list and the ground-truth list.
    *
    * @param response The response list of labels to compute score for.
    * @param simScore The word-similarity function to use (defaults to HSO).
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def SAF1(response: List[String], simScore: SimilarityScore = SimilarityScore.HSO): (Double, Double, Double) = {
    // Retrieve the gold distribution, and sort both distributions
    val res = response.sorted
    val gold = goldSet.toList.sorted

    // The pair-wise similarity scores between every response-gold pairs
    var scores: Array[Array[Double]] = Array[Array[Double]]()

    // Compute the pair-wise similarity scores using the appropriate similarity function
    simScore match {
      case SimilarityScore.HSO => scores = calcSimilarityMatrix(res, gold)
    }

    // Compute the WSA-F1
    SAF1(res, gold, scores)
  }

  /** Compute the Weighted Similarity-Aligned (WSA) F1 score between a response distribution
    * and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param gold The ground-truth distribution to compute score against.
    * @param responseMatches Array specifying the index of the "best-matching" gold
    *                        item for each response item.
    * @param goldMatches Array specifying the index of the "best-matching" response item
    *                    for each gold item.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def calcWSAF1(response: List[(String, Double)], gold: List[(String, Double)],
                   responseMatches: Array[Int], goldMatches: Array[Int], scores: Array[Array[Double]])
  : (Double, Double, Double) = {

    // Take care of edge case
    if (response.size == 0) {
      val p = 1.0; val r = 0.0; val f1 = Metrics.F1(p, r)
      return (f1, p, r)
    }

    // Compute similarity aligned precision & recall
    val precision = response.zipWithIndex.map(e => e._1._2 * scores(e._2)(responseMatches(e._2))).sum
    val recall = gold.zipWithIndex.map(e => e._1._2 * scores(goldMatches(e._2))(e._2)).sum
    val f1 = Metrics.F1(precision, recall)

    // Print log
    if (log) {
      println("Response-coverage: " + response.zipWithIndex.map(e =>
        (e._1, gold(responseMatches(e._2))._1, scores(e._2)(responseMatches(e._2)))).sortWith(_._3 > _._3)
        .mkString(", "))
    }
    if (log) {
      println("Gold-coverage: " + gold.zipWithIndex.map(e =>
        (e._1, response(goldMatches(e._2))._1, scores(goldMatches(e._2))(e._2))).sortWith(_._3 > _._3)
        .mkString(", "))
    }
    if (log) println("WSA Precision = " + precision)
    if (log) println("WSA Recall = " + recall)
    if (log) println("WSA F1 = " + f1)

    (f1, precision, recall)
  }

  /** Compute the Weighted Similarity-Aligned (WSA) F1 score between a response distribution
    * and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param gold The ground-truth distribution to compute score against.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *               If an empty array is given, new scores will be computed using the default
    *               word-similarity function.
    * @param uniformExperiment Also calculate the score base on a uniform response distribution
    *                          if true (for experimental purposes).
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def WSAF1(response: List[(String, Double)], gold: List[(String, Double)],
                   scores: Array[Array[Double]],
                   uniformExperiment: Boolean): (Double, Double, Double) = {
    // For each response label, find its best matching gold label (according to similarity score)
    val responseMaxMatches = VectorMath.maxColumnIdx(scores).map(_(0))

    // For each gold label, find its best matching response label (according to similarity score)
    val goldMaxMatches = VectorMath.maxRowIdx(scores).map(_(0))

    // Compute similarity-constraint F1 score with uniform distribution (just good to know)
    if (uniformExperiment) {
      val responseUniform = 1.0 / response.size
      if (log) println("\nUniform Weights (WSA)")
      calcWSAF1(response.map(e => (e._1, responseUniform)), gold, responseMaxMatches, goldMaxMatches, scores)
    }

    // Compute similarity-constraint F1 score
    if (log) println("\nDistribution Weights (WSA)")
    calcWSAF1(response, gold, responseMaxMatches, goldMaxMatches, scores)
  }

  /** Compute the Weighted Similarity-Aligned (WSA) F1 score between a response distribution
    * and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param simScore The word-similarity function to use (defaults to HSO).
    * @param uniformExperiment Also calculate the score base on a uniform response distribution
    *                          if true (for experimental purposes).  Set to false by default.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def WSAF1(response: List[(String, Double)],
                   simScore: SimilarityScore = SimilarityScore.HSO,
                   uniformExperiment: Boolean = false): (Double, Double, Double) = {
    // Retrieve the gold distribution, and sort both distributions
    val res = response.sortWith(_._2 > _._2)
    val gold = goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._2 > _._2)

    // The pair-wise similarity scores between every response-gold pairs
    var scores: Array[Array[Double]] = Array[Array[Double]]()

    // Compute the pair-wise similarity scores using the appropriate similarity function
    simScore match {
      case SimilarityScore.HSO => scores = calcSimilarityMatrix(res.map(_._1), gold.map(_._1))
    }

    // Compute the WSA-F1
    WSAF1(res, gold, scores, uniformExperiment)
  }

  /** Compute the Constrained Weighted & Similarity Aligned (CWSA) F1 score between a response
    * distribution and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param gold The ground-truth distribution to compute score against.
    * @param responseMatches Array specifying the index of the "best-matching" gold items
    *                        for each response item.
    * @param goldMatches Array specifying the index of the "best-matching" response items
    *                    for each gold item.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def calcCWSAF1(response: List[(String, Double)], gold: List[(String, Double)],
                               responseMatches: Array[List[Int]], goldMatches: Array[List[Int]],
                               scores: Array[Array[Double]])
  : (Double, Double, Double) = {

    // Take care of edge case
    if (response.size == 0) {
      val p = 1.0; val r = 0.0; val f1 = Metrics.F1(p, r)
      return (f1, p, r)
    }

    // For each label, find the max similarity score of the other set mapped to it
    val resSim = responseMatches.zipWithIndex.map(e => e._1.map(gIdx => scores(e._2)(gIdx)).max)
    val goldSim = goldMatches.zipWithIndex.map(e => e._1.map(rIdx => scores(rIdx)(e._2)).max)

    // For each label from one set, find the mass total of the other set mapped to it
    // Note: if the sim score is 0.0, we set the mass to 0.0 rather than leaving it at 1.0
    val resMass = responseMatches.map(gList => gList.map(gIdx => gold(gIdx)._2).sum)
    val goldMass = goldMatches.map(rList => rList.map(rIdx => response(rIdx)._2).sum)
    for (i <- 0 until resSim.size) {
      if (math.abs(resSim(i)) < Stats.DoubleEpsilon) {
        resMass(i) = 0.
      }
    }
    for (i <- 0 until goldSim.size) {
      if (math.abs(goldSim(i)) < Stats.DoubleEpsilon) {
        goldMass(i) = 0.
      }
    }


    // Compute weighted scores
    val resScores = response.zipWithIndex.map(e => {
      val (rIdx, rWeight) = (e._2, e._1._2); math.min(rWeight, resMass(rIdx)) * resSim(rIdx)
    })
    val goldScores = gold.zipWithIndex.map(e => {
      val (gIdx, gWeight) = (e._2, e._1._2); math.min(gWeight, goldMass(gIdx)) * goldSim(gIdx)
    })

    // Compute weighted & similarity aligned precision & recall
    val precision = resScores.sum
    val recall = goldScores.sum
    val f1 = Metrics.F1(precision, recall)

    // For each label from one set, find the labels of the other set mapped to it
    val resLabels = responseMatches.map(gList => gList.map(gIdx => gold(gIdx)._1))
    val goldLabels = goldMatches.map(rList => rList.map(rIdx => response(rIdx)._1))

    // Print log
    val sortedResMatches = response.zipWithIndex.map(e =>
      (e._1, resMass(e._2), resSim(e._2), (resLabels(e._2).size, resLabels(e._2).slice(0, 5).mkString(" ")),
        resScores(e._2)))
      .sortWith(_._5 > _._5).map(e => (e._1, e._2, e._3, e._4))
    val sortedGoldMatches = gold.zipWithIndex.map(e =>
      (e._1, goldMass(e._2), goldSim(e._2), (goldLabels(e._2).size, goldLabels(e._2).slice(0, 5).mkString(" ")),
        goldScores(e._2)))
      .sortWith(_._5 > _._5).map(e => (e._1, e._2, e._3, e._4))

    if (log) {
      println("Weighted Response-coverage: " + sortedResMatches.mkString(", "))
    }
    if (log) {
      println("Weighted Gold-coverage: " + sortedGoldMatches.mkString(", "))
    }
    if (log) println("CWSA Precision = " + precision)
    if (log) println("CWSA Recall = " + recall)
    if (log) println("CWSA F1 = " + f1)
    if (log) {
      println("res = [" + response.map(_._2).mkString(", ") + "]")
      println("resSim = [" + resSim.mkString(", ") + "]")
      println("resGoldMatch = [" + resMass.mkString(", ") + "]")
      println("gold = [" + gold.map(_._2).mkString(", ") + "]")
      println("goldSim = [" + goldSim.mkString(", ") + "]")
      println("goldResMatch = [" + goldMass.mkString(", ") + "]")
    }

    (f1, precision, recall)
  }

  /** Compute the Constrained Weighted Similarity-Aligned (CWSA) F1 score between a response distribution
    * and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param gold The ground-truth distribution to compute score against.
    * @param scores 2D matrix containing scores for all response-gold pairs, where response
    *               items are mapped along the rows and the gold items are mapped to columns.
    *               If an empty array is given, new scores will be computed using the default
    *               word-similarity function.
    * @param uniformExperiment Also calculate the score base on a uniform response distribution
    *                          if true (for experimental purposes).
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def CWSAF1(response: List[(String, Double)], gold: List[(String, Double)],
                           scores: Array[Array[Double]],
                           uniformExperiment: Boolean): (Double, Double, Double) = {
    // For each response label, find its best matching gold label (according to similarity score)
    val responseMaxMatches = VectorMath.maxColumnIdx(scores)

    // For each gold label, find its best matching response label (according to similarity score)
    val goldMaxMatches = VectorMath.maxRowIdx(scores)

    // Compute similarity-constraint F1 score with uniform distribution (just good to know)
    if (uniformExperiment) {
      val responseUniform = 1.0 / response.size
      if (log) println("\nUniform Weights (CWSA)")
      calcCWSAF1(response.map(e => (e._1, responseUniform)), gold,
        responseMaxMatches, goldMaxMatches, scores)
    }

    // Compute similarity-constraint F1 score
    if (log) println("\nDistribution Weights (CWSA)")
    calcCWSAF1(response, gold, responseMaxMatches, goldMaxMatches, scores)
  }

  /** Compute the Constrained Weighted Similarity-Aligned (CWSA) F1 score between a response distribution
    * and a ground-truth distribution.
    *
    * @param response The response distribution to compute score for.
    * @param simScore The word-similarity function to use (defaults to HSO).
    * @param uniformExperiment Also calculate the score base on a uniform response distribution
    *                          if true (for experimental purposes).  Set to false by default.
    *
    * @return Triplet of scores (F1, precision, recall).
    */
  def CWSAF1(response: List[(String, Double)],
                           simScore: SimilarityScore = SimilarityScore.HSO,
                           uniformExperiment: Boolean = false): (Double, Double, Double) = {
    // Retrieve the gold distribution, and sort both distributions
    val res = response.sortWith(_._2 > _._2)
    val gold = goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._2 > _._2)

    // The pair-wise similarity scores between every response-gold pairs
    var scores: Array[Array[Double]] = Array[Array[Double]]()

    // Compute the pair-wise similarity scores using the appropriate similarity function
    simScore match {
      case SimilarityScore.HSO => scores = calcSimilarityMatrix(res.map(_._1), gold.map(_._1), simScore = simScore)
    }

    // Compute the CWSA-F1
    CWSAF1(res, gold, scores, uniformExperiment)
  }

  /** Compute the vector distributional distance between a response distribution and a
    * ground-truth distribution by representing each distribution as a feature vector.
    *
    * Each distribution is converted to a vector by doing a linear combination of the
    * weighted elements in vector space.  The cosine-similarity and euclidean distance
    * between the two distribution vectors are computed.
    *
    * Note: The vector binary file must have been loaded.
    *
    * @param response The response distribution to compute score for.
    *
    * @return The vector distributional distance.
    */
  def vecDistributionalDistance(response: List[(String, Double)]): (Double, Double) = {
    if (!vectorsLoaded) throw new Exception("Vectors space not loaded!")

    // Find vector representation for the gold scores
    val goldDistribution = goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble))
    val goldVector = new Array[Float](vectors.vectorSize)
    var v: Array[Float] = null
    goldDistribution.foreach(g => {
      if (vectors.contains(g._1)) {
        v = vectors.vector(g._1)
        for (i <- 0 until goldVector.size)
          goldVector(i) += (g._2.toFloat * v(i))
      } else {
        println("Out of dictionary word from gold: " + g._1)
      }
    })

    // Find vector representation for the response scores
    val responseVector = new Array[Float](vectors.vectorSize)
    response.foreach(o => {
      if (vectors.contains(o._1)) {
        v = vectors.vector(o._1)
        for (i <- 0 until responseVector.size)
          responseVector(i) += (o._2.toFloat * v(i))
      } else {
        println("Out of dictionary word from response: " + o._1)
      }
    })

    // Should we normalize first?
    val cosine = vectors.cosine(goldVector, responseVector)
    val distance = vectors.euclidean(goldVector, responseVector)
    if (log) println("\nVectors distance = " + distance)
    if (log) println("Vectors cosine = " + cosine)

    (distance, cosine)
  }

}

/** Evaluation singleton object */
object Evaluation {

  /** Different similarity scoring functions:
    *
    *  - HSO (Hirst & St-Onge, 1998):
    *     Two lexicalized concepts are semantically close if their WordNet synsets are
    *     connected by a path that is not too long and that "does not change direction
    *     too often".
    *
    *  - W2V (word2vec):
    *     Two words are similar if the cosine angle between their respective vector
    *     representations is small.
    */
  object SimilarityScore extends Enumeration {
    type SimilarityScore = Value
    val HSO, W2V = Value
  }

  /** Find the average score for a list of F1 scores.
    * @param f1s List of F1 scores in the form of (F1, precision, recall) tuples.
    * @return The average F1 score tuple in the form of (F1, precision, recall).
    */
  def aveF1s(f1s: List[(Double, Double, Double)]): (Double, Double, Double) = {
    val f = f1s.map(_._1)
    val p = f1s.map(_._2)
    val r = f1s.map(_._3)
    (f.sum / f.size, p.sum / p.size, r.sum / r.size)
  }

  /** Pretty print the list of F1 scores.
    * @param f1s List of F1 scores in the form of (F1, precision, recall) tuples.
    */
  def printF1s(f1s: List[(Double, Double, Double)]) {
    val f = f1s.map(_._1)
    val p = f1s.map(_._2)
    val r = f1s.map(_._3)
    println("\nAvgP, AveR, AveF1\n" + ("=" * 17))
    println((p.sum / p.size) + ", " + (r.sum / r.size) + ", " + (f.sum / f.size))
    println("\nPrecisions\n" + p.mkString("\n"))
    println("\nRecalls\n" + r.mkString("\n"))
    println("\nF1s\n" + f.mkString("\n"))
  }

}
