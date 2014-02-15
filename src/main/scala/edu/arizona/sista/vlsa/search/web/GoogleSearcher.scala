package edu.arizona.sista.vlsa.search.web

import collection.JavaConversions._
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.customsearch.Customsearch
import com.google.api.services.customsearch.CustomsearchRequestInitializer
import com.google.api.services.customsearch.model.{Search, Result}
import com.martiansoftware.jsap.FlaggedOption
import com.martiansoftware.jsap.JSAP
import com.martiansoftware.jsap.JSAPResult
import com.martiansoftware.jsap.Switch
import edu.arizona.sista.vlsa.search.Searcher
import edu.arizona.sista.vlsa.utils.Constants
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

/** A web searcher that uses the Google Custom Searcher API to search web contents.
  *
  * Link to API details: https://developers.google.com/custom-search/
  * 
  * @constructor Create a web searcher using Google search.
  * @param apiKey The Google API access key to use.
  * @param engineUID The custom search engine ID to use.
  * @param params Additional search-specific parameters (stored as key-value pairs).
  * 
  * @author trananh
  */
class GoogleSearcher(
    var apiKey: String = Constants.GoogleProperties.apiKey,
    var engineUID: String = Constants.GoogleProperties.cseID,
    var params: mutable.HashMap[String,Any] = new mutable.HashMap[String,Any]())
  extends Searcher {

  // Build custom search engine
  val builder = new Customsearch.Builder(GoogleNetHttpTransport.newTrustedTransport(), new GsonFactory(), null)
  builder.setCustomsearchRequestInitializer(new CustomsearchRequestInitializer(apiKey))
  builder.setApplicationName(GoogleSearcher.APPLICATION_NAME)


  /** Perform a Google search.
    *
    * @param query The query.
    * @param numPageHits Maximum number of search hits per search (page). This value is
    *                    capped by Google's allowance (currently limited at 10).
    * @param startPoint The index of the first result to return.
    *
    * @return Google search response.
    */
  def search(query: String,
             numPageHits: Integer = GoogleSearcher.DEFAULT_MAX_PAGE_HITS,
             startPoint: Long = GoogleSearcher.DEFAULT_START_POINT): Search = {

    // Sanity check
    val num = Math.min(numPageHits, GoogleSearcher.DEFAULT_MAX_PAGE_HITS)

    // Build request
    val request = builder.build().cse().list(query)
    request.setCx(engineUID)
    request.setNum(num)
    request.setStart(startPoint)

    // Get results
    request.execute()
  }

  /** Perform a Google search and return all top results.
    *
    * @param query The query.
    * @param numHits Maximum number of top search hits to return. This value is
    *                capped by Google's allowance (currently limited at 100).
    *
    * @return Array of top Google search results.
    */
  def searchAllPages(query: String, numHits: Integer = GoogleSearcher.DEFAULT_MAX_HITS): Array[Result] = {
    val results = new ListBuffer[Result]()
    var startPoint = GoogleSearcher.DEFAULT_START_POINT
    var nextBatch = search(query, numPageHits = GoogleSearcher.DEFAULT_MAX_PAGE_HITS, startPoint = startPoint)
    breakable {
      while (results.length < numHits && nextBatch.getItems.size() > 0) {
        results.appendAll(nextBatch.getItems)
        startPoint = startPoint + nextBatch.getItems.size()
        if (startPoint > GoogleSearcher.DEFAULT_MAX_HITS)
          break
        nextBatch = search(query, numPageHits = GoogleSearcher.DEFAULT_MAX_PAGE_HITS, startPoint = startPoint)
      }
    }
    results.slice(0, numHits).toArray
  }

  /** Extract snippets from top Google search results.
    *
    * @param query The query.
    * @param numHits Maximum number of top search hits to return. This value is
    *                capped by Google's allowance (currently limited at 100).
    *
    * @return Array of formatted text snippets extracted from top Google search results.
    */
  def snippets(query: String, numHits: Integer = GoogleSearcher.DEFAULT_MAX_HITS): Array[String] = {
    searchAllPages(query, numHits = numHits).map(r => r.getSnippet)
  }

  /** Return the overall total number of hits estimated from the search.
    *
    * @param query The query.
    *
    * @return Estimated total number of hits.
    */
  def totalResults(query: String): Long = {
    this.search(query, numPageHits = 1).getQueries.get("request").get(0).getTotalResults
  }
  
  
  /******** Searcher implementation starts here ********/
  
  def close() { /* nothing to close for now */ }

  /** Return the term frequency (i.e., the total number of time the term is found).
    *
    * @param term The query term.
    *
    * @return Total occurrence count of the term.
    */
  def termFreq(term: String): Long = totalResults(term)

  /** Return the document frequency for the term.
    *
    * @param term The query term.
    *
    * @return Number of documents containing the term.
    */
  def docFreq(term: String): Long = totalResults(term)

  /** Search and return highlighted snippets from the results.
    *
    * @param queryStr The query string.
    * @param numHits Maximum number of search hits to return.
    *
    * @return Array of highlighted text snippets from the results.
    */
  def searchHighlight(queryStr: String, numHits: Integer): Array[String] = snippets(queryStr, numHits)

}

/** Google search singleton object */
object GoogleSearcher {

  /** Currently, we can only retrieve up to DEFAULT_MAX_PAGE_HITS number
    * of search hits at a time, for up to DEFAULT_MAX_HITS total hits overall
    * for any particular query.
    *
    * This is a limitation imposed by the current Google Custom Search API.
    */
  val DEFAULT_MAX_HITS = 100
  val DEFAULT_MAX_PAGE_HITS = 10
  val DEFAULT_START_POINT = 1
  val APPLICATION_NAME = "vlsa-application/1.0"
}

object RunGoogleSearcher {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("query")
      .setDefault("").setRequired(true).setLongFlag("query"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("query"))) {
      print("search-google ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("search-google --query chase")
      return
    }

    // Get argument
    val queryStr = config.getString("query").trim()

    // Search web
    val searcher = new GoogleSearcher()
    val snippets = searcher.snippets(queryStr)
    snippets.foreach(s => println(s))

    // Print summary statistics
    println("\nTop results: " + snippets.size)
    println("\nTotal results: " + searcher.totalResults(queryStr))
  }
  
}