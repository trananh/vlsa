package edu.arizona.sista.vlsa.demos

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.customsearch.Customsearch
import com.google.api.services.customsearch.CustomsearchRequestInitializer
import com.google.api.services.customsearch.model.Result
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL
import java.util

/** Perform a google search through the Google Custom Search
  * API using REST.
  *
  * @author trananh
  */
object GoogleSearchDemo {

  /**
    *********************************************************
    * !!! IMPORTANT: ADD YOUR API-KEY and ENGINE-ID !!!!
    *********************************************************
    */
  val seKey = "INSERT YOUR SEARCH-ENGINE KEY HERE"      // Constants.GoogleProperties.apiKey
  val apiKey = "INSERT YOUR API-KEY HERE"               // Constants.GoogleProperties.cseID


  /** Google search demo. */
  def main(args: Array[String]) {

    // Query string here
    val query = "chase"

    // Search using HTTP connection
    useHTTPConnection(query)

    // Search using Google Custom Search API
    useCustomSearchAPI(query)

  }

  /**
    * Using Java's native HttpConnection
    */
  def useHTTPConnection(query: String) {
    val url: URL = new URL("https://www.googleapis.com/customsearch/v1?key=" +
      apiKey + "&cx=" + seKey + "&q=" + query + "&alt=json")
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    conn.setRequestProperty("Accept", "application/json")
    val br = new BufferedReader(new InputStreamReader(conn.getInputStream))
    println("Output from Server ... \n")
    var output = Option(br.readLine())
    while (output.isDefined) {
      println(output.get)
      output = Option(br.readLine())
    }
    conn.disconnect()
  }

  /**
    * Using Google's custom search API
    */
  def useCustomSearchAPI(query: String) {
    val maxHits = 100
    val maxHitsPerSearch = 10

    // Build searcher
    val httpTransport = GoogleNetHttpTransport.newTrustedTransport()
    val jsonFactory = new GsonFactory()
    val initializer = new CustomsearchRequestInitializer(apiKey)

    val builder = new Customsearch.Builder(httpTransport, jsonFactory, null)
    builder.setCustomsearchRequestInitializer(initializer)
    val customSearch = builder.build()
    val request = customSearch.cse().list(query)
    request.setCx(seKey)
    request.setNum(maxHitsPerSearch)

    val results: java.util.List[Result] = new util.ArrayList[Result]()

    // Execute search
    var search = request.execute()
    results.addAll(search.getItems)
    while (search.getQueries.containsKey("nextPage") &&
      search.getQueries.get("nextPage").get(0).getStartIndex <= maxHits - maxHitsPerSearch + 1) {
      request.setStart(search.getQueries.get("nextPage").get(0).getStartIndex.toLong)
      search = request.execute()
      results.addAll(search.getItems)
    }

    println("Total results: " + search.getSearchInformation.getTotalResults)
    println("Showing the first " + results.size() + " results.")
    val it = results.iterator()
    var count = 0
    while (it.hasNext) {
      val r = it.next
      count += 1
      println("\nResult " + count)
      println("- Title: " + r.getTitle)
      println("- Snippet: " + r.getSnippet)
      println("- URL: " + r.getFormattedUrl)
    }

  }

}