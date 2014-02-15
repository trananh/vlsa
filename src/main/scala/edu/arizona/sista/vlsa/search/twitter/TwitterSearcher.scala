package edu.arizona.sista.vlsa.search.twitter

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.search.Searcher
import edu.arizona.sista.vlsa.utils.Constants
import scala.collection.JavaConversions._
import scala.collection.mutable
import twitter4j._
import twitter4j.conf.{Configuration, ConfigurationBuilder}

/** A Twitter searcher that uses the twitter4j API to search for relevant tweets.
  *
  * @constructor Create a Twitter searcher using twitter4j.
  * @param config The twitter connection configuration to use.
  * @param params Additional search-specific parameters (stored as key-value pairs).
  *
  * @author trananh
  */
class TwitterSearcher(
    var config: Configuration = new ConfigurationBuilder()
      .setDebugEnabled(Constants.DEBUG)
      .setOAuthConsumerKey(Constants.TwitterProperties.oauthConsumerKey)
      .setOAuthConsumerSecret(Constants.TwitterProperties.oauthConsumerSecret)
      .setOAuthAccessToken(Constants.TwitterProperties.oauthAccessToken)
      .setOAuthAccessTokenSecret(Constants.TwitterProperties.oauthAccessTokenSecret)
      .build(),
    var params: mutable.HashMap[String,Any] = new mutable.HashMap[String,Any]())
  extends Searcher {

  /** Creates a TwitterFactory with the given configuration. */
  val twitterFactory = new TwitterFactory(config)


  /** Returns tweets that match a specified query.
    * @param query The query.
    * @return List of tweet results.
    */
  def search(query: String): java.util.List[Status] = {
    val twitter = twitterFactory.getInstance()
    val result = twitter.search(new Query(query))
    result.getTweets
  }


  /******** Searcher implementation starts here ********/

  /** Close all open streams and clear memory usage */
  def close() { /* nothing to close for now */ }

  /** Return the term frequency (i.e., the total number of time the term is found).
    *
    * @param term The query term.
    *
    * @return Total occurrence count of the term.
    */
  def termFreq(term: String): Long = search(term).length

  /** Return the document frequency for the term.
    *
    * @param term The query term.
    *
    * @return Number of documents containing the term.
    */
  def docFreq(term: String): Long = search(term).length

  /** Search and return highlighted snippets from the results.
    *
    * @param queryStr The query string.
    * @param numHits Maximum number of search hits to return.
    *
    * @return Array of highlighted text snippets from the results.
    */
  def searchHighlight(queryStr: String, numHits: Integer): Array[String] = {
    val results = search(queryStr).map(status => "@" + status.getUser.getScreenName + ": " + status.getText)
    results.slice(0, numHits).toArray[String]
  }

}

/** Search twitter. */
object RunTwitterSearcher {

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
      print("search-twitter ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("search-twitter --query \"chase police\"")
      return
    }

    // Get argument
    val queryStr = config.getString("query").trim()

    // Search twitter and print results
    val searcher = new TwitterSearcher()
    val results = searcher.search(queryStr)
    results.foreach(status => println("@" + status.getUser.getScreenName + ": " + status.getText))
  }

}
