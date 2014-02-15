package edu.arizona.sista.vlsa.search.lucene

import com.martiansoftware.jsap.FlaggedOption
import com.martiansoftware.jsap.JSAP
import com.martiansoftware.jsap.JSAPResult
import com.martiansoftware.jsap.Switch
import edu.arizona.sista.vlsa.corpus
import edu.arizona.sista.vlsa.search.Searcher
import edu.arizona.sista.vlsa.utils.{NLPUtils, Constants}
import java.io.File
import java.io.IOException
import java.text.BreakIterator
import java.util.Date
import java.util.Locale
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.{MultiFields, DirectoryReader, Term}
import org.apache.lucene.search.highlight.Highlighter
import org.apache.lucene.search.highlight.QueryScorer
import org.apache.lucene.search.highlight.SimpleHTMLFormatter
import org.apache.lucene.search.highlight.SimpleSpanFragmenter
import org.apache.lucene.search.highlight.TokenSources
import org.apache.lucene.search.postingshighlight.PassageFormatter
import org.apache.lucene.search.postingshighlight.PassageScorer
import org.apache.lucene.search.postingshighlight.PostingsHighlighter
import org.apache.lucene.search.{TopDocs, Query, ScoreDoc}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/** An index searcher for searching through an index created with Lucene (v4.2.1).
  *
  * @constructor Create an index searcher.
  * @param idxDir Root directory of the index.
  * @param fieldName The default field for query.
  * @param analyzer Lucene analyzer (default to StandardAnalyzer).
  * @param version Lucene version.
  * @param params Additional search-specific parameters (stored as key-value pairs).
  *
  * @author trananh
  */
class IndexSearcher(val idxDir: File,
    var fieldName: String,
    val analyzer: Analyzer = new StandardAnalyzer(Version.LUCENE_42), 
    val version: Version = Version.LUCENE_42,
    var params: mutable.HashMap[String,Any] = new mutable.HashMap[String,Any]())
  extends Searcher {

  def this(idxPath: String, fieldName: String) = this(new File(idxPath), fieldName)

  /** Readers and searchers */
  val reader = DirectoryReader.open(FSDirectory.open(idxDir))
  val searcher = new org.apache.lucene.search.IndexSearcher(reader)

  /** Hits from the most recent search */
  var hits: Array[ScoreDoc] = Array[ScoreDoc]()


  /** Set the default field for query. */
  def setFieldName(fn: String) {
    fieldName = fn
  }

  /** Return the document hit corresponding to the ID.
    * @param docID Hit ID of the document.
    * @return A document matching the query.
    */
  def getHit(docID: Integer): Document = {
    searcher.doc(docID)
  }

  /** Search index.
    * @param query The query.
    * @param numHits Maximum number of hits (default to a large number).
    * @return The top hitting documents by score.
    */
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def search(query: Query, numHits: Integer = IndexSearcher.DEFAULT_MAX_HITS): Array[ScoreDoc] = {
    val start = new Date()
    if (Constants.DEBUG) println("Searching started for: " + query)
    hits = searcher.search(query, numHits).scoreDocs
    if (Constants.DEBUG) {
      println("Found " + hits.length + " hits")
      println("Search completed. Total time: " + (new Date().getTime - start.getTime) + " milliseconds")
    }
    hits
  }

  /** Highlight search results using Lucene's standard Highlighter.
    *
    * The Highlighter does not require term vectors, but will run faster
    * with the vectors.
    *
    * @param query The query.
    * @param fieldName The default field for query.
    * @param fragmentSize Size in bytes of each highlight fragment (default 100).
    * @param mergeContiguousFragments Merge continuous fragments (default false).
    * @param maxNumFragments The maximum number of fragments (default 5).
    * @param numHits Maximum number of hits (default to a large number).
    * @param preTag Tag to insert before each hit in the highlight.
    * @param postTag Tag to insert after each hit in the highlight.
    *
    * @return Highlighted text fragments (between 0 and maxNumFragments
    *         number of fragments).
    */
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def highlightStandard(query: Query,
    fieldName: String = this.fieldName,
    fragmentSize: Integer = params.getOrElse("fragmentSize", IndexSearcher.DEFAULT_FRAG_SIZE).asInstanceOf[Integer],
    mergeContiguousFragments: Boolean = params.getOrElse("mergeContiguousFragments", false).asInstanceOf[Boolean],
    maxNumFragments: Integer = params.getOrElse("maxNumFragments", 5).asInstanceOf[Integer],
    numHits: Integer = IndexSearcher.DEFAULT_MAX_HITS,
    preTag: String = params.getOrElse("preTag", IndexSearcher.DEFAULT_HIGHLIGHT_PRETAG).asInstanceOf[String],
    postTag: String = params.getOrElse("postTag", IndexSearcher.DEFAULT_HIGHLIGHT_POSTTAG).asInstanceOf[String])
  : Array[String] = {

    val start = new Date()
    if (Constants.DEBUG) println("Highlighting started for: " + query)

    // Query index
    hits = searcher.search(query, numHits).scoreDocs

    // Highlight search
    val scorer = new QueryScorer(query)
    val highlighter = new Highlighter(new SimpleHTMLFormatter(preTag, postTag), scorer)
    highlighter.setTextFragmenter(new SimpleSpanFragmenter(scorer, fragmentSize))
    val highlights = new ArrayBuffer[String]()
    for (i <- 0 until hits.length) {
      val doc = getHit(hits(i).doc)
      val text = doc.get(fieldName)
      val tokenStream = TokenSources.getAnyTokenStream(searcher.getIndexReader, hits(i).doc, fieldName, analyzer)
      val frags = highlighter.getBestTextFragments(tokenStream, text, mergeContiguousFragments, maxNumFragments)
      for (j <- 0 until frags.length) {
        if (Option(frags(j)).isDefined && frags(j).getScore > 0)
          highlights += frags(j).toString
      }
    }

    if (Constants.DEBUG) {
      println("Found " + highlights.length + " hits")
      println("Highlighting completed. Total time: " + (new Date().getTime - start.getTime) + " milliseconds")
    }

    highlights.toArray[String]
  }

  /** Highlight search results using Lucene's experimental PostingsHighlighter
    * with added windowing capability.  PostingsHighlighter is optimized
    * to run faster than the standard highlighter, but is experimental.
    *
    * Windowing allows the user to specify how many preceding and proceeding
    * passages to return, in addition to the passage containing the hit.
    *
    * The PostingsHighlighter does not analyze fields nor use term vectors,
    * but it does require IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS.
    *
    * <b>WARNING</b>: The code is only guaranteed to be compatible with
    * LUCENE version 4.2.
    *
    * @param query The query.
    * @param fieldName The default field for query.
    * @param window The window of passages before & after the hit passage (default 1).
    * @param numHits Maximum number of search hits (default to a large number).
    * @param maxLength Maximum content size to process (default to a large number).
    * @param maxPassages The maximum number of top-N ranked passages used to 
    *                    form the highlighted snippets (default to a large number).
    * @param preTag Tag to insert before each hit in the highlight.
    * @param postTag Tag to insert after each hit in the highlight.
    * @param ellipsis Tag to insert between each hit in the highlight.
    * @param bi Method of breaking the content into passages (default to sentences).
    *
    * @return Array of formatted snippets corresponding to the hit documents.
    */
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def highlightPostings(query: Query,
    fieldName: String = this.fieldName,
    window: Integer = params.getOrElse("window", 1).asInstanceOf[Integer],
    numHits: Integer = IndexSearcher.DEFAULT_MAX_HITS,
    maxLength: Integer = params.getOrElse("maxLength", IndexSearcher.DEFAULT_MAX_LENGTH).asInstanceOf[Integer],
    maxPassages: Integer = params.getOrElse("maxPassages", IndexSearcher.DEFAULT_MAX_PASSAGES).asInstanceOf[Integer],
    preTag: String = params.getOrElse("preTag", IndexSearcher.DEFAULT_HIGHLIGHT_PRETAG).asInstanceOf[String],
    postTag: String = params.getOrElse("postTag", IndexSearcher.DEFAULT_HIGHLIGHT_POSTTAG).asInstanceOf[String],
    ellipsis: String = params.getOrElse("ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String],
    bi: BreakIterator = params.getOrElse("bi", BreakIterator.getSentenceInstance(Locale.ROOT)).asInstanceOf[BreakIterator])
  : Array[String] = {

    // Sanity check
    val win = Math.max(window, 0)

    val start = new Date()
    if (Constants.DEBUG) println("Highlighting (using postings) started for: " + query)

    // Query index
    val topDocs = searcher.search(query, numHits)

    // Highlight using WindowPassageFormatter to get window of passages
    val wpf = new WindowPassageFormatter(win, preTag, postTag, ellipsis, bi)
    val highlighter = new PostingsHighlighter(maxLength, bi, new PassageScorer(), wpf.asInstanceOf[PassageFormatter])
    val highlights = highlighter.highlight(fieldName, query, searcher, topDocs, maxPassages)

    if (Constants.DEBUG) {
      println("Found " + highlights.length + " hits")
      println("Highlighting completed. Total time: " + (new Date().getTime - start.getTime) + " milliseconds")
    }

    highlights
  }

  /** Highlight search results using Lucene's experimental PostingsHighlighter
    * with added windowing capability.  PostingsHighlighter is optimized
    * to run faster than the standard highlighter, but is experimental.
    *
    * NOTE: Results are retrieved (lazily) in batches as a stream for efficiency.
    * Each batch can be viewed as a page of results.
    *
    * Windowing allows the user to specify how many preceding and proceeding
    * passages to return, in addition to the passage containing the hit.
    *
    * The PostingsHighlighter does not analyze fields nor use term vectors,
    * but it does require IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS.
    *
    * <b>WARNING</b>: The code is only guaranteed to be compatible with
    * LUCENE version 4.2.
    *
    * @param query The query.
    * @param after The doc for which all returned results should be after.
    * @param fieldName The default field for query.
    * @param window The window of passages before & after the hit passage (default 1).
    * @param numHitsPerPage Maximum number of search hits (default to a large number) per batch/page.
    * @param maxLength Maximum content size to process (default to a large number).
    * @param maxPassages The maximum number of top-N ranked passages used to
    *                    form the highlighted snippets (default to a large number).
    * @param preTag Tag to insert before each hit in the highlight.
    * @param postTag Tag to insert after each hit in the highlight.
    * @param ellipsis Tag to insert between each hit in the highlight.
    * @param bi Method of breaking the content into passages (default to sentences).
    *
    * @return Stream of array of formatted snippets corresponding to the hit documents.
    */
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  def highlightPostingsStream(query: Query,
    after: Option[ScoreDoc] = None,
    fieldName: String = this.fieldName,
    window: Integer = params.getOrElse("window", 1).asInstanceOf[Integer],
    numHitsPerPage: Integer = IndexSearcher.DEFAULT_MAX_HIGHLIGHTS_PAGE,
    maxLength: Integer = params.getOrElse("maxLength", IndexSearcher.DEFAULT_MAX_LENGTH).asInstanceOf[Integer],
    maxPassages: Integer = params.getOrElse("maxPassages", IndexSearcher.DEFAULT_MAX_PASSAGES).asInstanceOf[Integer],
    preTag: String = params.getOrElse("preTag", IndexSearcher.DEFAULT_HIGHLIGHT_PRETAG).asInstanceOf[String],
    postTag: String = params.getOrElse("postTag", IndexSearcher.DEFAULT_HIGHLIGHT_POSTTAG).asInstanceOf[String],
    ellipsis: String = params.getOrElse("ellipsis", IndexSearcher.DEFAULT_HIGHLIGHT_ELLIPSIS).asInstanceOf[String],
    bi: BreakIterator = params.getOrElse("bi", BreakIterator.getSentenceInstance(Locale.ROOT)).asInstanceOf[BreakIterator])
  : Stream[Array[String]] = {

    // Sanity check
    val win = Math.max(window, 0)

    var topDocs: TopDocs = null
    if (!after.isDefined) {
      topDocs = searcher.search(query, numHitsPerPage)
    } else {
      topDocs = searcher.searchAfter(after.get, query, numHitsPerPage)
    }

    if (topDocs.scoreDocs.size == 0)
      return Stream.empty

    if (Constants.DEBUG)
      println("Retrieved " + topDocs.scoreDocs.size + "/" + topDocs.totalHits + " highlights.")

    // Highlight using WindowPassageFormatter to get window of passages
    val wpf = new WindowPassageFormatter(win, preTag, postTag, ellipsis, bi)
    val highlighter = new PostingsHighlighter(maxLength, bi, new PassageScorer(), wpf.asInstanceOf[PassageFormatter])
    val highlights = highlighter.highlight(fieldName, query, searcher, topDocs, maxPassages)

    return highlights #:: highlightPostingsStream(query, after = Option(topDocs.scoreDocs.last),
                                                  fieldName = fieldName, window = window, numHitsPerPage = numHitsPerPage,
                                                  maxLength = maxLength, maxPassages = maxPassages,
                                                  preTag = preTag, postTag = postTag, ellipsis = ellipsis,
                                                  bi = bi)

  }

  /** Get the sum of the total frequency of all terms.  In other words, it's the total number
    * of tokens in the index.
    *
    * @return The sum of the total frequency of all terms.
    */
  def getSumTotalTermFreq(): Long = {
    MultiFields.getTerms(reader, fieldName).getSumTotalTermFreq
  }

  /******** Searcher implementation starts here ********/

  /** Close all open streams and clear memory usage */
  def close() {
    reader.close()
  }

  /** Return the term frequency (i.e., the total number of time the term is found).
    *
    * @param term The query term.
    *
    * @return Total occurrence count of the term.
    */
  def termFreq(term: String): Long = {
    val analyzed = NLPUtils.analyzeLucene(term, analyzer = this.analyzer)
    reader.totalTermFreq(new Term(fieldName, analyzed.mkString("-")))
  }

  /** Return the document frequency for the term.
    *
    * @param term The query term.
    *
    * @return Number of documents containing the term.
    */
  def docFreq(term: String): Long = {
    val analyzed = NLPUtils.analyzeLucene(term, analyzer = this.analyzer)
    reader.docFreq(new Term(fieldName, analyzed.mkString("-")))
  }

  /** Search and return highlighted snippets from the results.
    *
    * @param queryStr The query string.
    * @param numHits Maximum number of search hits to return.
    *
    * @return Array of highlighted text snippets from the results.
    */
  def searchHighlight(queryStr: String, numHits: Integer): Array[String] = {
    val query = QueryBuilder.mkQuery(queryStr, this.fieldName, this.analyzer, this.version)
    val h = params.getOrElse("highlighter", IndexSearcher.Highlighter.Postings)
    if (h.asInstanceOf[IndexSearcher.Highlighter.Value] == IndexSearcher.Highlighter.Postings) {
      // Use postings highlighter
      highlightPostings(query, numHits = numHits)
    } else {
      // Use standard highlighter
      highlightStandard(query, numHits = numHits)
    }
  }
  
}

object IndexSearcher {

  /** Default values for all search */
  val DEFAULT_MAX_HITS = Int.MaxValue - 1       // maximum number of hits to return (490811)
  val DEFAULT_MAX_HIGHLIGHTS_PAGE = 250000      // maximum number of highlights to return per page (for 4GB memory)

  /** Default values for postings highlighter */
  val DEFAULT_MAX_LENGTH = Int.MaxValue - 1     // maximum length of a doc to examine
  val DEFAULT_MAX_PASSAGES = 1000		            // number of passages to return per doc

  /** Default values for highlighters */
  val DEFAULT_FRAG_SIZE = 200                   // number of characters per fragment
  val DEFAULT_MAX_FRAG = 5                      // maximum number of fragments per doc
  val DEFAULT_HIGHLIGHT_PRETAG = "<b>"          // pre-tag for search term
  val DEFAULT_HIGHLIGHT_POSTTAG = "</b>"        // post-tag for search term
  val DEFAULT_HIGHLIGHT_ELLIPSIS = "..."        // delimiter between disconnected passages from the same doc

  object Highlighter extends Enumeration {
    type Highlighter = Value
    val Standard, Postings = Value
  }
}

object RunIndexSearcher {

  def highlights(searcher: IndexSearcher, query: Query): Array[String] = {
    val highlights = searcher.highlightPostings(query)
    println("Found " + highlights.size + " highlights total")
    println(highlights.slice(0, 5).mkString("\n"))
    highlights
  }

  def highlightsStream(searcher: IndexSearcher, query: Query): Array[String] = {
    val highlights = new ListBuffer[String]()
    searcher.highlightPostingsStream(query, numHitsPerPage = 500).foreach(h => {
      println("=> Processing " + highlights.size + " - " + (highlights.size + h.size) + " highlights.")
      highlights.appendAll(h)
    })
    println("Found " + highlights.size + " highlights total")
    println(highlights.slice(0, 5).mkString("\n"))
    highlights.toArray
  }

  def querySearch(searcher: IndexSearcher, query: Query) {
    // Search index
    val hits = searcher.search(query)

    // Print out information for the first hit
    if (hits.length > 0) {
      println("\nInfo for the first 5 hits:\n")
      for (i <- 0 until (math.min(5, hits.length))) {
        val luceneDoc = searcher.getHit(hits(i).doc)
        println(corpus.Document.fromLuceneDocument(luceneDoc).toString())
      }
    } else {
      println("No hits!")
    }
  }

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("index-dir")
      .setDefault("").setRequired(true).setLongFlag("index-dir"))
    jsap.registerParameter(new FlaggedOption("query")
      .setDefault("").setRequired(true).setLongFlag("query"))
    jsap.registerParameter(new FlaggedOption("field")
      .setDefault("").setRequired(true).setLongFlag("field"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("index-dir"))
      || "".equals(config.getString("query"))
      || "".equals(config.getString("field"))) {
      print("search-index ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("search-index --index-dir /path/to/data/text/indexes/Gigaword-stemmed --query chase --field text")
      return
    }

    // Get arguments
    val indexDir = config.getString("index-dir").trim()
    val queryStr = config.getString("query").trim()
    val fieldName = config.getString("field").trim()

    val searcher = new IndexSearcher(new File(indexDir),
      fieldName, analyzer = new EnglishAnalyzer(Version.LUCENE_42))
    val query = QueryBuilder.mkQuery(queryStr, fieldName,
      analyzer = searcher.analyzer, version = searcher.version)

    // Query
    querySearch(searcher, query)

    // Highlights
    val h = highlightsStream(searcher, query)

    searcher.close()
  }
  
}