package edu.arizona.sista.vlsa.demos

import scala.collection.JavaConversions._
import twitter4j._
import twitter4j.conf.ConfigurationBuilder

/** Demo of basic twitter search/streaming functions using twitter4j.
  * @author trananh
  */
object Twitter4jDemo {

  /** Twitter Demo */
  def main(args: Array[String]) {

    /**
     *********************************************************
     * !!! IMPORTANT: ADD SECRET KEYS BELOW !!!!
     *********************************************************
     */

    // Build configuration
    val config = new ConfigurationBuilder()
      .setDebugEnabled(true)
      .setOAuthConsumerKey("*********************")
      .setOAuthConsumerSecret("******************************************")
      .setOAuthAccessToken("**************************************************")
      .setOAuthAccessTokenSecret("******************************************")
      .build()

    // Search twitter
    search(new TwitterFactory(config), "chase police")

    // Stream twitter (with filter)
    stream(new TwitterStreamFactory(config), Array("chase", "police"))
  }

  /** Search Twitter */
  def search(tf: TwitterFactory, queryStr: String) = {
    val twitter = tf.getInstance()
    val query = new Query(queryStr)
    val result = twitter.search(query)
    for (status <- result.getTweets()) {
      println("@" + status.getUser.getScreenName + ": " + status.getText)
    }
  }

  /** Stream twitter tweets with a filter query */
  def stream(ts: TwitterStreamFactory, query: Array[String]) = {
    println("\n\n*****\nStarting stream\n*****\n\n")
    Thread.sleep(2000)

    // Set up listener
    val listener = new StatusListener {
      def onStatus(status: Status) {
        println(status.getUser.getName + ": " + status.getText)
      }
      def onException(ex: Exception) { ex.printStackTrace() }
      def onStallWarning(warning: StallWarning) {}
      def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
      def onScrubGeo(userId: Long, upToStatusId: Long) {}
      def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
    }

    // Init twitter stream
    val twitterStream = ts.getInstance()
    twitterStream.addListener(listener)

    // Set up filter query
    val fq = new FilterQuery()
    fq.track(query)

    // sample()/filter() internally creates a thread which manipulates TwitterStream
    // and calls the adequate listener methods continuously.
    twitterStream.filter(fq)

    // Stream for 10 seconds and stop.
    Thread.sleep(10000)

    twitterStream.shutdown()
    twitterStream.cleanUp()
  }

}
