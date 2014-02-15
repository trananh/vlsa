package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.DocNeighborhood.Scoring.Scoring
import edu.arizona.sista.vlsa.search.lucene.{QueryBuilder, IndexSearcher}
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import edu.arizona.sista.vlsa.utils.{NLPUtils, Constants}
import java.io.File
import java.util.regex.Pattern
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** A simple word-search model to find the occurrence frequency of keywords within
  * some neighborhood window of the search results.  This model operates specifically
  * on results retrieved from an index corpus.
  *
  * @constructor A word-neighborhood model that operates on an indexed corpus.
  * @param searcher Searcher to use to retrieve relevant text.
  *
  * @author trananh
  */
class DocNeighborhood(val searcher: IndexSearcher) {

  /** Maintain a map of each keyword to its base form. */
  val keywordsMap = new mutable.HashMap[String, String]()

  /** Dictionary of words that we're interested in */
  val dictionary = new FrequencyDictionary[String](N = 2)

  /** CoreNLP processor */
  val processor: Processor = new CoreNLPProcessor(internStrings = false)

  /** Clear all open streams and internal memory usage. */
  def clear() {
    searcher.close()
    dictionary.clear()
    keywordsMap.clear()
  }

  /** Reset frequency count for all tokens. */
  def reset() {
    dictionary.reset()
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
    */
  def loadDictionary(dictFile: File) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTerms(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      dictionary.createEntry(e._2)
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
    */
  def loadAdjectiveDictionary(dictFile: File) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTermsAsAdjectives(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      dictionary.createEntry(e._2)
      keywordsMap.put(e._1, e._2)
    })
    source.close()
  }

  /** A more memory-efficient method of retrieving highlights that does not load all highlights
    * into memory at once. Highlights are retrieved from the index in batches rather than all
    * at once.  A cached file is required for this method.
    *
    * @param query The query string.
    * @param numHitsPerPage Max number of query results (passages) to process per batch.
    * @param cacheFile Path to file containing the cached highlights.  If the file does not already
    *                  exist, a new search will be performed and the results cached to the file.
    *                  We need to use a cache file for efficiency, to help lazily load the highlights
    *                  so that we don't need to keep all highlight passages in memory.
    *
    * @return A stream of string iterators to iterate through each search highlight.
    */
  def getHighlightsStream(query: String, numHitsPerPage: Integer, cacheFile: String): Iterator[String] = {

    // Load highlights from a cached file as a stream of a single iterator.
    def loadCacheFile(file: File): Iterator[String] = {
      assert(file.exists())
      return scala.io.Source.fromFile(file).getLines()
    }

    // Load cached data if already exists
    val file = new File(cacheFile)
    if (file.exists()) {
      return loadCacheFile(file)
    }
    if (Constants.DEBUG) println("Cache file not found! Performing new index search.")

    // Perform new index search of no cache is found
    val q = QueryBuilder.mkQuery(query, searcher.fieldName, searcher.analyzer, searcher.version)
    val highlightBatches = searcher.highlightPostingsStream(q, numHitsPerPage = numHitsPerPage)

    // Cache the new results so that we can re-load it with an iterator
    val out = new java.io.FileWriter(file)
    var first = true
    highlightBatches.foreach(batch => {
      batch.foreach(h => {
        if (first) {
          out.write(h)
          first = false
        }
        else
          out.write("\n" + h)
      })
    })
    out.close

    // Return the loaded file
    loadCacheFile(file)
  }

  /** Retrieve the highlights (aka snippets) by searching the index or loading from
    * a cache file provided.
    *
    * @param query The query string.
    * @param numHits Max number of query results (passages) to process in total.
    * @param cacheFile Path to file containing the cached highlights from a previous search.
    *                  Caching search results can speed up computation significantly.  Setting
    *                  this value to None (default) will trigger a new search, where as setting
    *                  this value to a non-existing file-path will trigger a new search whose
    *                  results will be cached to the specified path.
    *
    * @return A string iterator to iterate through each search highlight.
    */
  def getHighlights(query: String, numHits: Integer, cacheFile: Option[String] = None): Iterator[String] = {
    if (cacheFile.isDefined) {
      // Load cached data if exists
      val file = new File(cacheFile.get)
      if (file.exists()) {
        return scala.io.Source.fromFile(file).getLines()
      }
      if (Constants.DEBUG) println("Cache file not found! Performing new index search.")
    }

    // Perform new index search of no cache is found
    val highlights = searcher.searchHighlight(query, numHits)

    // Cache the highlights (line by line for efficiency)
    if (cacheFile.isDefined) {
      val out = new java.io.FileWriter(new File(cacheFile.get))
      if (highlights.size > 0) {
        out.write(highlights(0))
        for (i <- 1 until highlights.size) {
          out.write("\n" + highlights(i))
        }
      }
      out.close
    }

    highlights.iterator
  }

  /** Scan the neighborhood of text generated from searching the given query string and keep
    * count of keywords from the dictionary.
    *
    * - retrieve search results
    * - run (tokenize + lemmatize) on found passages
    * - update count for keyword tokens found in each passage
    *
    * @param queryStr The query string.
    * @param numHits Max number of query results (passages) to process in total.
    * @param cacheFile Path to file containing the cached highlights from a previous search.
    *                  Caching search results can speed up computation significantly.  Setting
    *                  this value to None (default) will trigger a new index search.
    */
  def processQueryStr(queryStr: String, numHits: Integer, cacheFile: Option[String] = None) {
    if (Constants.DEBUG) println("WNH general search: CoreNLP tokenize + lemmatize")

    // Retrieve search highlights
    val highlights = getHighlights(queryStr, numHits, cacheFile)

    // Process highlights
    var doc: Document = null
    val targets = new ListBuffer[String]()
    val ellipsis = searcher.params.getOrElse("ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String]
    highlights.foreach(h => h.split(Pattern.quote(ellipsis)).foreach(passage => {
      doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(passage))
      NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)
      targets.clear()
      doc.sentences.foreach(s => {
        targets.appendAll(s.lemmas.get.map(_.toLowerCase))
      })
      dictionary.increment(targets, Option(passage))
    }))

  }

  /** Scan the neighborhood of text generated from searching the given query string and keep
    * count of keywords from the dictionary, take care only to count a keyword from the search
    * if it fits the correct part-of-speech requirement.
    *
    * - retrieve search results
    * - run (tokenize + lemmatize + POS-tagging) on found passages
    * - for each passage, filter for tokens matching specific POS labels
    * - update count for keyword tokens found
    *
    * @param queryStr The query string.
    * @param numHits Max number of query results (passages) to process in total.
    * @param targetPOSLabels Set of POS labels used to filter the tokens
    *                        (default to adjective labels).
    * @param cacheFile Path to file containing the cached highlights from a previous search.
    *                  Caching search results can speed up computation significantly.  Setting
    *                  this value to None (default) will trigger a new index search.
    */
  def processQueryStrTargetPOS(queryStr: String, numHits: Integer,
                               targetPOSLabels: Set[String] = NLPUtils.POS_JJs,
                               cacheFile: Option[String] = None) {
    if (Constants.DEBUG) println("WNH simple-search: CoreNLP tokenize + lemmatize + POS-targets")

    // Retrieve search highlights
    val highlights = getHighlights(queryStr, numHits, cacheFile)

    // Process highlights
    var doc: Document = null
    val targets = new ListBuffer[String]()
    val ellipsis = searcher.params.getOrElse("ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String]
    highlights.foreach(h => h.split(Pattern.quote(ellipsis)).foreach(passage => {
      doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(passage))
      NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)
      targets.clear()
      doc.sentences.foreach(s => {
        for (i <- 0 until s.lemmas.get.length) {
          if (s.tags.isDefined && targetPOSLabels.contains(s.tags.get(i)))
            targets += s.lemmas.get(i).toLowerCase
        }
      })
      dictionary.increment(targets, Option(passage))
    }))

  }

  /** Scan the neighborhood of text generated from searching for the specified terms and
    * keep count of keywords from the dictionary.  The term and the keyword must appear
    * in the correct part-of-speech for the keyword to be counted.
    *
    * - retrieve search results
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
    * @param numHits Max number of query results (passages) to process in total.
    * @param targetPOSLabels Set of POS labels used to filter the tokens
    *                        (default to adjective labels).
    * @param cacheDir Path to directory containing the cached highlights from a previous search.
    *                 Caching search results can speed up computation significantly.  Setting
    *                 this value to None (default) will trigger a new index search.
    */
  def processSearchTermsWithPOS(searchTerms: List[(String, Option[Set[String]])], numHits: Integer,
                                targetPOSLabels: Set[String] = NLPUtils.POS_JJs,
                                cacheDir: Option[String] = None) {
    if (Constants.DEBUG) println("WNH simple-search: CoreNLP tokenize + lemmatize + POS-terms + POS-targets")

    // Create cache file name
    val cacheFilename = searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt"
    var cacheFile: Option[String] = None
    if (cacheDir.isDefined)
      cacheFile = Option(cacheDir.get + "/" + cacheFilename)

    // Retrieve search highlights
    val queryStr = searchTerms.map("\"" + _._1.trim + "\"").mkString(" AND ")
    val highlights = getHighlights(queryStr, numHits, cacheFile)

    // Lemmatize the search terms
    val searchLemmas = searchTerms.map(e => {
      (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0), e._2)
    }).toMap

    // Process highlights
    var doc: Document = null
    val targets = new ListBuffer[String]()
    val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)
    val ellipsis = searcher.params.getOrElse("ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String]
    highlights.foreach(h => h.split(Pattern.quote(ellipsis)).foreach(passage => {
      // Iterate through each highlight and annotate
      doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(passage))
      NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)
      // Filter for search terms appearing in the correct POS for _each_ sentence in the highlight.
      doc.sentences.foreach(s => {
        targets.clear()
        searchFlags.keys.foreach(k => searchFlags.put(k, false))
        for (i <- 0 until s.lemmas.get.length) {
          if (s.tags.isDefined) {
            if (targetPOSLabels.contains(s.tags.get(i)))
              targets += s.lemmas.get(i).toLowerCase
            if (searchLemmas.contains(s.lemmas.get(i).toLowerCase)) {
              if (!searchLemmas.get(s.lemmas.get(i).toLowerCase).get.isDefined ||
                searchLemmas.get(s.lemmas.get(i).toLowerCase).get.get.contains(s.tags.get(i)))
                searchFlags.put(s.lemmas.get(i).toLowerCase, true)
            }
          }
        }
        if (searchFlags.values.reduceLeft(_ && _)) {
          // If all search terms are found in the correct POS, then update the frequency counts
          // for all keywords found in the highlight.
          dictionary.increment(targets, Option(s.words.mkString(" ")))
        }
      })
    }))

  }

  /** Scan the neighborhood of text generated from searching for the specified terms and
    * keep count of the number of valid results in which the terms appear
    * in the correct part-of-speech.
    *
    * This method requires caching to files.
    *
    * - retrieve search results
    * - run (tokenize + lemmatize + POS) on found passages
    * - filter for passages where the search terms match specific POS labels
    * - update frequency count for each valid match
    *
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param cacheDir Path to directory containing the cached highlights from a previous search.
    *
    * @return A count of the number of valid results.
    */
  def countTermsWithPOS(searchTerms: List[(String, Option[Set[String]])], cacheDir: String): Long = {
    // Create cache file name
    val cacheFilename = searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt"
    val cacheHighlightsFile = cacheDir + "/" + cacheFilename

    // Retrieve search highlights
    val queryStr = searchTerms.map("\"" + _._1.trim + "\"").mkString(" AND ")
    val highlights = getHighlightsStream(queryStr, IndexSearcher.DEFAULT_MAX_HIGHLIGHTS_PAGE, cacheHighlightsFile)

    // Lemmatize the search terms
    val searchLemmas = searchTerms.map(e => {
      (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0), e._2)
    }).toMap

    // The frequency count
    var frequency = 0L

    // Process highlights
    var doc: Document = null
    val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)
    val ellipsis = searcher.params.getOrElse(
      "ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String]
    highlights.foreach(h => h.split(Pattern.quote(ellipsis)).foreach(passage => {
      // Iterate through each highlight and annotate
      doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(passage))
      NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)
      // Filter for search terms appearing in the correct POS for _each_ sentence in the highlight.
      doc.sentences.foreach(s => {
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
          // If all search terms are found in the correct POS, then update the frequency count by 1.
          frequency += 1
        }
      })
    }))

    frequency
  }

  /** Return the current results as a list of (word, score) pairs, ranked by the scores.
    * The scoring method defaults to using the relevance-rate but it can be changed.
    * See [[edu.arizona.sista.vlsa.models.text.ie.neighborhood.DocNeighborhood.Scoring]] for details.
    *
    * @param scoringMethod The scoring method to use, defaults to using the relevance-rate.
    * @param frequency Map of terms to their respective frequency.  Defaults
    *                  to using frequency counts from the internal dictionary of the model.
    * @return List of (word, score) pairs from the dictionary, ranked by the scores.
    */
  def getRankedResults(scoringMethod: Scoring = DocNeighborhood.Scoring.RelevanceRate,
                       frequency: mutable.Map[String, Double] =
                       this.dictionary.freq(0).map(entry => (entry._1(0), entry._2.toDouble)))
    : List[(String, Double)] = {

    // Filter out items with 0 frequency
    val validFrequency = frequency.toList.filter(_._2 > 0).sortWith(_._2 > _._2)

    scoringMethod match {

      case DocNeighborhood.Scoring.Frequency => return validFrequency

      case DocNeighborhood.Scoring.NormalizedFrequency => {
        val sum = validFrequency.map(_._2).sum
        return validFrequency.map(e => (e._1, e._2 / sum))
      }

      case DocNeighborhood.Scoring.RelevanceRate => {
        /* Relevance-rate scoring:
         *   score = (rel-freq / total-freq) * log(rel-freq)
         */
        var (relFreq, totalFreq, score) = (0.0, 0.0, 0.0)
        val relRank = validFrequency.map(entry => {
          relFreq = entry._2 + 1.0
          totalFreq = this.searcher.termFreq(entry._1) + 1.0
          score = (relFreq / totalFreq) * math.log(relFreq)
          (entry._1, score)
        })
        return relRank.sortWith(_._2 > _._2)
      }

    }
    throw new Exception("Unrecognized scoring method specified.")
  }

}


/** Document neighborhood singleton object */
object DocNeighborhood {

  /** Different methods for scoring results, where
    *  - rel-freq(wi) is the number of times word wi is found in the search context.
    *  - total-freq(wi) is the total number of times word wi is found in the corpus.
    *
    * Frequency:
    *   score(wi) = rel-freq(wi)
    *
    * NormalizedFrequency:
    *   score(wi) = rel-freq(wi) / [rel-freq(w1) + rel-freq(w2) + ... + rel-freq(wN)]
    *
    * RelevanceRate:
    *   score(wi) = [rel-freq(wi) / total-freq(wi)] * log(rel-freq(wi))
    */
  object Scoring extends Enumeration {
    type Scoring = Value
    val Frequency, NormalizedFrequency, RelevanceRate = Value
  }

  /** Construct and return a new Document Neighborhood.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @param indexDir Root directory of the index.
    * @param window The window of passages before & after the hit passage (default 0).
    * @param preTag Tag to insert before each hit in the highlight (default "").
    * @param postTag Tag to insert after each hit in the highlight (default "").
    * @param ellipsis Tag to insert between each hit in the highlight (default "...").
    * @param fieldName The default field for query (default "text").
    * @param analyzer Lucene analyzer (default to EnglishAnalyzer).
    * @param version Lucene version.
    * @return A new DocNeighborhood model.
    */
  def createModel(statesDictionaryFile: String, indexDir: String,
                  window: Integer = 0, preTag: String = "",
                  postTag: String = "", ellipsis: String =  "...",
                  fieldName: String = "text",
                  analyzer: Analyzer = new EnglishAnalyzer(Version.LUCENE_42),
                  version: Version = Version.LUCENE_42)
  : DocNeighborhood = {
    // Init additional params for highlighter
    val params = new mutable.HashMap[String, Any]()
    params.put("window", window)
    params.put("preTag", preTag)
    params.put("postTag", postTag)
    params.put("ellipsis", ellipsis)

    // Init searcher instance
    val searcher = new IndexSearcher(new File(indexDir), fieldName,
      analyzer = analyzer,
      version = version,
      params = params)

    // Instantiate new model
    val model = new DocNeighborhood(searcher)
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile))

    model
  }
}


/** Perform an index search on the specified search context save the highlights
  * to file for offline use.
  */
object RunSearchHighlightsToFile {

  /** Perform search and write the resulting highlights to file. */
  def searchToFile(outDir: String, index: String, searchContext: Array[String], overwrite: Boolean = true) {
    val filename = outDir + "/" + searchContext.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt"

    // Do nothing if file already exists and no overwrite permission is given, else remove
    // the file so we can overwrite.
    val cacheFile = new File(filename)
    if (cacheFile.exists()) {
      if (!overwrite)
        return
      cacheFile.delete()
    }

    // The query
    val queryStr = searchContext.map("\"" + _.toLowerCase.trim + "\"").mkString(" AND ")

    // Create model and search
    val model = DocNeighborhood.createModel("", index)
    model.getHighlights(queryStr, IndexSearcher.DEFAULT_MAX_HITS, cacheFile = Option(filename))
  }

  /** Formulate all possible search patterns from the detections and perform search,
    * saving all results to file.
    */
  def main(args: Array[String]) {
    val outDir = "/Volumes/MyPassport/data/vlsa/neighborhood/gigaword"
    val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
    val annotationDir = "/Volumes/MyPassport/data/text/data/ground-truth-pilot"

    val annotationSet = (1 to 4).map(annotationDir + "/chase" + "%02d".format(_) + ".xml")
    annotationSet.foreach(annotationFile => {

      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood("")
      wnm.loadDetections(annotationFile)
      val queries = wnm.formulateQueriesAAL()

      queries.toList.sorted.foreach(q => {
        var searchContext = Array(q._1)
        println("Searching for: " + searchContext.mkString(", "))
        searchToFile(outDir, index, searchContext, overwrite = false)

        searchContext = Array(q._1, q._2)
        println("Searching for: " + searchContext.mkString(", "))
        searchToFile(outDir, index, searchContext, overwrite = false)

        searchContext = Array(q._1, q._2, q._3)
        println("Searching for: " + searchContext.mkString(", "))
        searchToFile(outDir, index, searchContext, overwrite = false)
      })

    })
  }

}


/** Run Document Neighborhood demo */
object RunDocNeighborhood {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("index-dir")
      .setDefault("").setRequired(true).setLongFlag("index-dir"))
    jsap.registerParameter(new FlaggedOption("mental-states")
      .setDefault("").setRequired(true).setLongFlag("mental-states"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("index-dir"))
      || "".equals(config.getString("mental-states"))) {
      print("RunDocNeighborhood ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("RunDocNeighborhood --index-dir /path/to/index/Gigaword-stemmed --mental-states states-adjectives.txt")
      return
    }

    // Get arguments
    val index = config.getString("index-dir").trim()
    val mentalStatesFile = config.getString("mental-states").trim()

    // Create a DNH model
    val model = DocNeighborhood.createModel(mentalStatesFile, index)

    // Process model
    val searchTerms = List(("chase", Option(NLPUtils.POS_VBs)), ("police", Option(NLPUtils.POS_NNs)))
    model.processSearchTermsWithPOS(searchTerms, IndexSearcher.DEFAULT_MAX_HITS)

    // Print contents of dictionary
    println("\n\n" + model.dictionary.prettyPrint(", "))

    // Print out some example passages (attributes)
    val N = 5
    println("\n\nSample passages:")
    model.dictionary.toSortedList().slice(0, N).foreach(entry => {
      println(" => " + entry._1(0))
      println(model.dictionary.attributes(0).get(entry._1).get.slice(0, N).mkString("\n"))
    })

    // Print scores
    println("\nRelevance-rate scores:\n" +
      model.getRankedResults(DocNeighborhood.Scoring.RelevanceRate).mkString(", "))

    // Clean up memory
    model.clear()
  }

}
