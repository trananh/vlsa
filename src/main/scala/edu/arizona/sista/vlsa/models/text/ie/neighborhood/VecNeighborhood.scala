package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.VecNeighborhood.Scoring
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.VecNeighborhood.Scoring.Scoring
import edu.arizona.sista.vlsa.models.text.word.Word2Vec
import edu.arizona.sista.vlsa.utils.NLPUtils
import java.io.File
import scala.collection.mutable

/** A simple word-search model that uses the vectors representation given by
  * the word2vec toolkit.  The idea is to find words, from a list of keywords, that
  * are closest in vector space to some search terms.
  *
  * @constructor A word-neighborhood model that operates in vector space.
  * @param vectorsBinFile Binary file of the trained word2vec model.
  *
  * @author trananh
  */
class VecNeighborhood(val vectorsBinFile: String) {

  /** Maintain a map of each keyword to its base form. */
  val keywordsMap = new mutable.HashMap[String, String]()

  /** Maintain a dictionary of each word and its associated score. */
  val scores = new mutable.HashMap[String, Double]()

  /** CoreNLP processor */
  val processor: Processor = new CoreNLPProcessor(internStrings = false)

  /** Instantiate a new word2vec model and load the binary data */
  val vecModel = new Word2Vec()
  vecModel.load(vectorsBinFile)

  /** Clear all open streams and internal memory usage. */
  def clear() {
    scores.clear()
    keywordsMap.clear()
    vecModel.clear()
  }

  /** Reset score for all tokens.
    * @param defaultScore The default score to set for each word.
    *                     Set to VecNeighborhood.DEFAULT_SCORE when using Euclidean distance mode.
    */
  def reset(defaultScore: Double = VecNeighborhood.DEFAULT_SCORE) {
    scores.keySet.foreach(k => scores(k) = defaultScore)
  }

  /** Load list of keywords that we're interested in.
    * All keywords are passed through lemmatization to recover the base form.
    *
    * Example file contents:
    * word1
    * .
    * .
    * wordN
    *
    * @param dictFile File containing the list of words.
    * @param defaultScore The default score to associate with each word loaded.
    *                     Set to VecNeighborhood.DEFAULT_SCORE when using Euclidean distance mode.
    */
  def loadDictionary(dictFile: File, defaultScore: Double = VecNeighborhood.DEFAULT_SCORE) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTerms(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      scores.put(e._2, defaultScore)
      keywordsMap.put(e._1, e._2)
    })
    source.close()
  }

  /** Load list of keyword adjectives that we're interested in.
    * All keywords are passed through lemmatization to recover the base form.
    *
    * Example file contents:
    * adjective1
    * .
    * .
    * adjectiveN
    *
    * @param dictFile File containing the list of adjectives.
    * @param defaultScore The default score to associate with each adjective loaded.
    */
  def loadAdjectiveDictionary(dictFile: File, defaultScore: Double = VecNeighborhood.DEFAULT_SCORE) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTermsAsAdjectives(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      scores.put(e._2, defaultScore)
      keywordsMap.put(e._1, e._2)
    })
    source.close()
  }

  /** Compute a search context vector by aggregating the vectors of the search terms.
    * This context vector represents the search context formed by the given
    * search terms. The distance between each keyword to this context vector
    * is computed.
    *
    * @param queryTerms List of search terms.
    * @param distance The distance method to use, choose between Euclidean vs Cosine
    *                 (defaults to Euclidean).
    */
  def process(queryTerms: List[String], distance: Scoring = VecNeighborhood.Scoring.ExpEuclideanBased) {
    // Make sure each query term has a vector representation
    queryTerms.foreach(term => {
      if (!vecModel.contains(term)) {
        println("Out of dictionary query term: " + term)
        return
      }
    })

    // Get each keyword distance to the sum-vector of the search terms
    var score = 0.0
    val searchVec = vecModel.normalize(vecModel.sumVector(queryTerms.toList))
    scores.keySet.foreach(keyword => {
      score = 0.0
      distance match {
        case Scoring.ExpCosineBased => score = vecModel.cosine(vecModel.vector(keyword), searchVec)
        case Scoring.ExpEuclideanBased => score = vecModel.euclidean(vecModel.vector(keyword), searchVec)
        case _ => throw new Exception("Unrecognized scoring function.")
      }

      scores.put(keyword, score)
    })
  }

  /** Return the current results as a list of (word, score) pairs, ranked by the scores.
    * The scoring method defaults to using the euclidean-based score but it can be changed.
    * See [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.VecNeighborhood.Scoring]] for details.
    *
    * @param scoringMethod The scoring method to use, defaults to using the euclidean-based score.
    * @param scores Map of term scores.  Defaults to using the internal scores map of the model.
    *
    * @return List of (word, score) pairs from the dictionary, ranked by the scores.
    */
  def getRankedResults(scoringMethod: Scoring = VecNeighborhood.Scoring.ExpEuclideanBased,
                       scores: mutable.Map[String, Double] = this.scores): List[(String, Double)] = {

    // Filter out items with DEFAULT_SCORE
    val validScores = scores.toList.filter(_._2 > VecNeighborhood.DEFAULT_SCORE).sortWith(_._2 > _._2)

    scoringMethod match {

      case VecNeighborhood.Scoring.Score => return validScores

      case VecNeighborhood.Scoring.ExpEuclideanBased => {
        /* Exponential euclidean-based scoring (euclidean range [0, 1]):
         *   score = exp(2.0 - euclidean-distance) - 1.0
         */
        return validScores.map(e => (e._1, math.expm1(2.0 - e._2))).sortWith(_._2 > _._2)
      }

      case VecNeighborhood.Scoring.ExpCosineBased => {
        /* Exponential cosine-based scoring (cosine-sim range [-1, 1]):
         *   score = exp(cosine-sim + 1.0) - 1.0
         */
        return validScores.map(e => (e._1, math.expm1(e._2 + 1.0))).sortWith(_._2 > _._2)
      }

    }
    throw new Exception("Unrecognized scoring method specified.")
  }

}

/** Vector neighborhood singleton object */
object VecNeighborhood {

  /** The default score. Should be set to something lower than any attainable score.
    * For EuclideanDistance, the lowest attainable score is 0.0.
    * For CosineSimilarity, the lowest attainable score is -1.0.
    */
  val DEFAULT_SCORE: Double = -10.0

  /** Different methods for scoring results.
    *
    * Score:
    *   score(wi)
    *
    * ExpEuclideanBased:
    *   score(wi) = exp(2 - euclidean-distance(wi)) - 1.0
    */
  object Scoring extends Enumeration {
    type Scoring = Value
    val Score, ExpEuclideanBased, ExpCosineBased = Value
  }

  /** Construct and return a new Vector Neighborhood model.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @param vectorsBinFile Binary file of the trained word2vec model.
    * @return A new VecNeighborhood model.
    */
  def createModel(statesDictionaryFile: String, vectorsBinFile: String): VecNeighborhood = {
    val model = new VecNeighborhood(vectorsBinFile)
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile), VecNeighborhood.DEFAULT_SCORE)
    model
  }

}

/** Run Vector Neighborhood demo */
object RunVecNeighborhood {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("vecs-bin")
      .setDefault("").setRequired(true).setLongFlag("vecs-bin"))
    jsap.registerParameter(new FlaggedOption("mental-states")
      .setDefault("").setRequired(true).setLongFlag("mental-states"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("vecs-bin"))
      || "".equals(config.getString("mental-states"))) {
      print("RunVecNeighborhood ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("RunVecNeighborhood --vecs-bin story-vecs.bin --mental-states states-adjectives.txt")
      return
    }

    // Get arguments
    val vectorsBinFile = config.getString("vecs-bin").trim()
    val statesDictionaryFile = config.getString("mental-states").trim()

    // Create a VNH model
    val model = VecNeighborhood.createModel(statesDictionaryFile, vectorsBinFile)

    // Process model
    model.process(List("chase", "police"))

    // Print scores
    println("\nScores:\n" + model.getRankedResults().mkString(", "))

    // Clean up memory
    model.clear()
  }

}
