package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import com.google.gson.Gson
import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.{Document, Processor}
import edu.arizona.sista.vlsa.search.web.{BingWebResult, BingSearcher}
import edu.arizona.sista.vlsa.utils.{Constants, NLPUtils}
import java.io.File
import scala.collection.mutable
import scala.util.control.Breaks._
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.WebNeighborhood.Scoring.Scoring

/** A simple word-search model to find the occurrence frequency of keywords within
  * some neighborhood window of the search results.  This model operates specifically
  * on results retrieved from a web search.  The frequency counts are used to
  * compute a relevance rate score for the each keyword.
  *
  * @constructor A word-neighborhood model that operates on web search results.
  * @param searcher Searcher to use to retrieve relevant text (using Bing search).
  *
  * @author trananh
  */
class WebNeighborhood(val searcher: BingSearcher) {

  /** Maintain a map of each keyword to its base form. */
  val keywordsMap = new mutable.HashMap[String, String]()

  /** Maintain a dictionary of each word (in its lemma form) and its associated score. */
  val scores = new mutable.HashMap[String, Double]()

  /** CoreNLP processor */
  val processor: Processor = new CoreNLPProcessor(internStrings = false)

  /** Clear all open streams and internal memory usage. */
  def clear() {
    searcher.close()
    scores.clear()
    keywordsMap.clear()
  }

  /** Reset score for all tokens.
    * @param defaultScore The default score to set for each word.
    */
  def reset(defaultScore: Double = WebNeighborhood.DEFAULT_SCORE) {
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
    */
  def loadDictionary(dictFile: File, defaultScore: Double = WebNeighborhood.DEFAULT_SCORE) {
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
  def loadAdjectiveDictionary(dictFile: File, defaultScore: Double = WebNeighborhood.DEFAULT_SCORE) {
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

  /** Perform a web search and save all results (as JSON) to a file.
    * The filename is automatically created from the search terms.
    *
    * @param searchTerms The search query.
    * @param outDir Directory to output the search results.
    */
  def searchToFile(searchTerms: Array[String], outDir: String, overwrite: Boolean = true) {
    // Create filename based on the search terms, also add dir if not
    val parent = outDir + "/" + searchTerms.dropRight(1).map(_.toLowerCase()).mkString("-")
    val filename = parent + "/" + searchTerms.map(_.toLowerCase()).mkString("-") + ".txt"

    // Do nothing if file already exists and no overwrite permission is given
    if (new File(filename).exists() && !overwrite)
      return

    // Create parent dir if needed
    val parentDir = new File(parent)
    if (!parentDir.exists())
      parentDir.mkdir()

    // Create the query string
    val queryString = searchTerms.map("\"" + _ + "\"").mkString(" ")

    // Web search
    val totalResults = searcher.totalResults(queryString)
    val results = searcher.searchAllPages(queryString)
    if (results.size < 500) println("Found less than 500 results (" + results.size + ") for " + filename)

    // Write to file
    val gson = new Gson()
    val writer = new java.io.FileWriter(filename)
    writer.write(queryString + "\n")
    writer.write(totalResults.toString + "\n")
    for (i <- 0 until results.length) {
      writer.write(gson.toJson(results(i)) + "\n")
    }
    writer.close()
  }

  /** Scan the neighborhood of text generated from searching for the specified terms and
    * keep count of keywords from the dictionary.  The term and the keyword must appear
    * in the correct part-of-speech for the keyword to be counted.
    *
    * The frequency count is used to compute a relevance-rate score for each term, where
    * the score is defined as:
    *
    * score(w) = [num-good-results / num-top-results] * log(total-results)
    *
    * - retrieve search results (from offline files)
    * - run (tokenize + lemmatize + POS) on found passages
    * - filter for passages where the search terms match specific POS labels
    * - for each passage, filter for tokens matching specific POS labels
    * - update count for keyword tokens found
    *
    * Note: Due to lemmatization, searches containing multiple terms that reduce to the same
    * lemma form can cause confusion to the algorithm, resulting in potentially unpredictable
    * behaviors.
    *
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param dataDir Path to directory containing search results pulled from the web.
    * @param targetPOSLabels Set of POS labels used to filter the tokens
    *                        (default to adjective labels).
    */
  def searchTermsWithPOS(searchTerms: List[(String, Option[Set[String]])], dataDir: String,
                         targetPOSLabels: Set[String] = NLPUtils.POS_JJs) {
    // Init some variables
    val prefix = searchTerms.map(_._1).mkString("-")
    var dataFile: File = null
    var score: Double = 0.0
    var stateLemma: String = ""

    // Iterate over each mental state
    keywordsMap.keySet.foreach(state => {
      // Get the lemma form of the state
      stateLemma = keywordsMap.get(state).get

      // Create filename based on the search terms + the mental state
      dataFile = new File(dataDir + "/" + prefix + "/" + prefix + "-" + state + ".txt")

      // Check if offline search results exist for the mental state
      if (dataFile.exists()) {

        // Lemmatize the search terms and add the mental state as another search criterion
        val searchLemmas = (searchTerms :+ (stateLemma, Option(targetPOSLabels))).map(e => {
          (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0), e._2)
        }).toMap

        // Open data file for reading
        val src = scala.io.Source.fromFile(dataFile)
        val linesIterator = src.getLines()
        val queryString = linesIterator.next()
        val totalResults = linesIterator.next().toLong.toFloat
        if (Constants.DEBUG) println("\n\nQuery String: " + queryString)

        // Process search snippets from the data file
        var doc: Document = null
        var result: BingWebResult = null
        var matches, numResults = 0f
        val gson = new Gson()
        val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)
        src.getLines().foreach(line => {
          // Iterate through each JSON search result
          if (line.trim().length > 0) {
            numResults += 1f
            result = gson.fromJson[BingWebResult](line, classOf[BingWebResult])

            // NLP annotate the snippet
            doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(result.Description))
            NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)

            breakable {
              doc.sentences.foreach(s => {
                // For each sentence in the snippet, filter for search terms appearing in the correct POS.
                searchFlags.keys.foreach(k => searchFlags.put(k, false))
                for (i <- 0 until s.lemmas.get.length) {
                  if (s.tags.isDefined) {
                    if (searchLemmas.contains(s.lemmas.get(i).toLowerCase)) {
                      if (!searchLemmas.get(s.lemmas.get(i).toLowerCase).get.isDefined ||
                        searchLemmas.get(s.lemmas.get(i).toLowerCase).get.get.contains(s.tags.get(i)))
                        searchFlags.put(s.lemmas.get(i).toLowerCase, true)
                    }
                  }
                }

                if (searchFlags.values.reduceLeft(_ && _)) {
                  // If all search terms are found in the correct POS, then count this search result
                  // as a match and break (no need to look at subsequent sentences in the snippet).
                  matches += 1f
                  break
                }
              })
            }
          }
        })

        // Close file source
        src.close()

        // Compute the score using a form of relevance-rate
        // score(w) = [num-good-results / num-top-results] * log(total-web-results)
        score = (matches / numResults) * math.log(totalResults)

      } else {
        // Else if there's no data for this search, then score = 0.0
        score = 0.0
      }

      // Store the score, keyed by the lemma form of the mental state.
      scores.put(stateLemma, score)

    })

  }

  /** Return the current results as a list of (word, score) pairs, ranked by the scores.
    * The scoring method defaults to using the raw score but it can be changed.
    * See [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.WebNeighborhood.Scoring]] for details.
    *
    * @param scoringMethod The scoring method to use, defaults to using the raw score.
    * @param scores Map of term scores.  Defaults to using the internal scores map of the model.
    *
    * @return List of (word, score) pairs from the dictionary, ranked by the scores.
    */
  def getRankedResults(scoringMethod: Scoring = WebNeighborhood.Scoring.Score,
                       scores: mutable.Map[String, Double] = this.scores): List[(String, Double)] = {

    // Filter out items with DEFAULT_SCORE
    val validScores = scores.toList.filter(_._2 > WebNeighborhood.DEFAULT_SCORE).sortWith(_._2 > _._2)

    scoringMethod match {

      case WebNeighborhood.Scoring.Score => return validScores

    }
    throw new Exception("Unrecognized scoring method specified.")
  }

}


/** Web neighborhood singleton object */
object WebNeighborhood {

  /** The default score. Should be set to something lower than any attainable score. */
  val DEFAULT_SCORE: Double = 0.0

  /** Different methods for scoring results.
    *
    * Score:
    *   score(wi)
    */
  object Scoring extends Enumeration {
    type Scoring = Value
    val Score = Value
  }

  /** Construct and return a new Web Neighborhood model.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @return A new WebNeighborhood model.
    */
  def createModel(statesDictionaryFile: String) : WebNeighborhood = {
    val model = new WebNeighborhood(new BingSearcher())
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile))
    model
  }

}


/** Perform a web search on the specified search context paired with specific adjectives
  * and save the results to file for offline use.
  */
object RunWebSearchToFile {
  def main(args: Array[String]) {
    val outDir = "/Volumes/MyPassport/data/vlsa/neighborhood/bing"
    val model = new WebNeighborhood(new BingSearcher())
    val adjectives = Array("angry", "desperate", "determined", "excited",
      "frustrating", "happy", "sad", "threatening", "violent",
      "afraid", "confused", "crazy", "enraged",
      "hostile", "inebriated", "playful", "terrified", "tired")
    val searchContext = Array("chase")
    adjectives.foreach(adj => {
      println("Processing: " + adj)
      model.searchToFile(searchContext :+ adj, outDir, overwrite = false)
      Thread.sleep(1000)  /* sleep for 1 second between search */
    })
  }
}


/** Perform a web search on the specified search context paired with adjectives loaded from a
  * dictionary and save the results to file for offline use.
  */
object RunWebSearchAllToFile {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("query")
      .setDefault("").setRequired(true).setLongFlag("query"))
    jsap.registerParameter(new FlaggedOption("mental-states")
      .setDefault("").setRequired(true).setLongFlag("mental-states"))
    jsap.registerParameter(new FlaggedOption("out-dir")
      .setDefault("").setRequired(true).setLongFlag("out-dir"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("query"))
      || "".equals(config.getString("mental-states"))
      || "".equals(config.getString("out-dir"))) {
      print("RunWebSearchAllToFile ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("RunWebSearchAllToFile --query \"chase police\" --mental-states states-adjectives.txt" +
        " --out-dir /path/to/data/bing")
      return
    }

    // Get arguments
    val searchContext = config.getString("query").trim().split(" ")
    val mentalStatesFile = config.getString("mental-states").trim()
    val outDir = config.getString("out-dir").trim()

    // Create new model, and start performing searches, saving results to files
    val model = new WebNeighborhood(new BingSearcher())
    val source = scala.io.Source.fromFile(new File(mentalStatesFile))
    source.getLines().foreach(line => {
      println("Processing: " + line.trim)
      model.searchToFile(searchContext :+ line.trim, outDir, overwrite = false)
      Thread.sleep(1000)  /* sleep for 1 second between search */
    })
    source.close
  }

}


/** Run Web Neighborhood demo */
object RunWebNeighborhood {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("data-dir")
      .setDefault("").setRequired(true).setLongFlag("data-dir"))
    jsap.registerParameter(new FlaggedOption("mental-states")
      .setDefault("").setRequired(true).setLongFlag("mental-states"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("data-dir"))
      || "".equals(config.getString("mental-states"))) {
      print("RunWebNeighborhood ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("RunWebNeighborhood --data-dir /path/to/data/bing --mental-states states-adjectives.txt")
      return
    }

    // Get arguments
    val dataDir = config.getString("data-dir").trim()
    val mentalStatesFile = config.getString("mental-states").trim()

    // Create a WNH model
    val model = new WebNeighborhood(new BingSearcher())

    // Load with default mental states
    model.loadAdjectiveDictionary(new File(mentalStatesFile))

    // Process model
    val searchTerms = List(("chase", Option(NLPUtils.POS_VBs)), ("police", Option(NLPUtils.POS_NNs)))
    model.searchTermsWithPOS(searchTerms, dataDir)

    // Print scores
    println("\nScores:\n" + model.scores.toList.sortWith(_._2 > _._2).mkString(", "))

    // Clean up memory
    model.clear()
  }

}
