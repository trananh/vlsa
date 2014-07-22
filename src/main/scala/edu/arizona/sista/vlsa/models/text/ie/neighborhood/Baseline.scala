package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.utils.NLPUtils
import java.io.File
import scala.collection.mutable

/** A naive/simple baseline model.
  *
  * @constructor A naive/simple baseline model.
  *
  * @author trananh
  */
class Baseline {

  /** Maintain a map of each keyword to its base form. */
  val keywordsMap = new mutable.HashMap[String, String]()

  /** Maintain a dictionary of each word (in its lemma form) and its associated score. */
  val scores = new mutable.HashMap[String, Double]()

  /** CoreNLP processor */
  val processor: Processor = new CoreNLPProcessor(internStrings = false)

  /** Clear all open streams and internal memory usage. */
  def clear() {
    scores.clear()
    keywordsMap.clear()
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
    * @param defaultScore The score to associate with each word loaded (defaults to 0).
    */
  def loadDictionary(dictFile: File, defaultScore: Double = 0) {
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
    * @param defaultScore The score to associate with each word loaded (defaults to 0).
    */
  def loadAdjectiveDictionary(dictFile: File, defaultScore: Double = 0) {
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

  /** Reset the scores for all tokens to 0.
    */
  def reset() = setScores(0)

  /** Set the scores for all tokens.
    * @param score The score to set for each word.
    */
  def setScores(score: Double) {
    scores.keySet.foreach(k => scores(k) = score)
  }

  /** Read a cached score from file.
    *
    * @param file File to read from.
    * @return Score read from file.
    */
  protected def readCacheScores(file: File): Long = {
    val source = scala.io.Source.fromFile(file)
    val line = source.getLines().next()
    source.close()
    line.toLong
  }

  /** Load the scores for all tokens from cached files.
    *
    * @param cacheDir Directory containing all cached scores.
    */
  def loadCachedScores(cacheDir: File) {
    scores.keySet.foreach(k => {
      val filename = cacheDir.getAbsolutePath + "/" + k.toLowerCase.trim + ".txt"
      val s = readCacheScores(new File(filename))
      scores.put(k, s)
    })
  }

  /** Return the current results as a list of (word, score) pairs, ranked by the scores.
    *
    * @param scores Map of term scores.  Defaults to using the internal scores map of the model.
    *
    * @return List of (word, score) pairs from the dictionary, ranked by the scores.
    */
  def getRankedResults(scores: mutable.Map[String, Double] = this.scores): List[(String, Double)] =
    scores.toList.sortWith(_._2 > _._2)

}

/** Baseline singleton object */
object Baseline {

  /** Construct and return a new baseline model.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @return A new baseline model.
    */
  def createModel(statesDictionaryFile: String) : Baseline = {
    val model = new Baseline()
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile))
    model
  }

}
