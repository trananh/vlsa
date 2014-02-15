package edu.arizona.sista.vlsa.search.twitter

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.utils.Constants
import java.io.{FileWriter, IOException, File}
import twitter4j._
import twitter4j.conf.{Configuration, ConfigurationBuilder}

/** A Twitter streamer that uses the twitter4j API to constantly retrieve relevant
  * tweets.
  *
  * @constructor Create a Twitter streamer using twitter4j.
  * @param config The twitter connection configuration to use.
  *
  * @author trananh
  */
class TwitterStreamer(
    var config: Configuration = new ConfigurationBuilder()
      .setDebugEnabled(Constants.DEBUG)
      .setOAuthConsumerKey(Constants.TwitterProperties.oauthConsumerKey)
      .setOAuthConsumerSecret(Constants.TwitterProperties.oauthConsumerSecret)
      .setOAuthAccessToken(Constants.TwitterProperties.oauthAccessToken)
      .setOAuthAccessTokenSecret(Constants.TwitterProperties.oauthAccessTokenSecret)
      .build()) {

  /** Creates a TwitterStreamFactory with the given configuration. */
  val streamFactory = new TwitterStreamFactory(config)


  /** Streams relevant tweets from twitter.
    * @param query Any filter restrictions on the stream (default to None).
    * @param outFile A file to redirect the stream into (default to None).
    */
  def stream(query: Option[Array[String]] = None, outFile: Option[File] = None) {

    if (Constants.DEBUG) println("\n\n*****\nStarting stream\n*****\n\n")
    if (outFile.isDefined) {
      if (!outFile.get.exists()) outFile.get.createNewFile()
      if (Constants.DEBUG) println("Streaming to file " + outFile.get.getPath + "\n")
    }
    Thread.sleep(3000)

    // Setup listener to print out incoming tweets.
    val listener = new StatusListener {

      def onStatus(status: Status) {
        val statusMessage = "@" + status.getUser.getScreenName + ": " + status.getText
        if (outFile.isDefined) {
          // Write to outfile, if it's defined.
          try {
            val writer = new FileWriter(outFile.get, true)
            writer.write(statusMessage)
            writer.write("\n")
            writer.close()
          } catch {
            case ioe: IOException => println("IOException: " + ioe.getMessage())
            case e: Exception => println(e.printStackTrace())
          }
        } else {
          // If no outfile, then print to console.
          println(statusMessage)
        }
      }

      def onException(ex: Exception) { ex.printStackTrace() }

      def onStallWarning(warning: StallWarning) {}
      def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
      def onScrubGeo(userId: Long, upToStatusId: Long) {}
      def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
    }

    // Initialize stream
    val twitterStream = streamFactory.getInstance()
    twitterStream.addListener(listener)

    // Create if a filtered stream if necessary.
    if (query.isDefined) {
      // Set up filter query
      val filterQuery = new FilterQuery().track(query.get)

      // filter() internally creates a thread which manipulates TwitterStream
      // and calls the adequate listener methods continuously.
      twitterStream.filter(filterQuery)
    } else {
      // sample() internally creates a thread which manipulates TwitterStream
      // and calls the adequate listener methods continuously.
      twitterStream.sample()
    }
  }

}

/** Stream Twitter. */
object RunTwitterStreamer {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("query")
      .setDefault("").setRequired(false).setLongFlag("query"))
    jsap.registerParameter(new FlaggedOption("out-file")
      .setDefault("").setRequired(false).setLongFlag("out-file"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")) {
      print("stream-twitter ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("stream-twitter --query \"chase police\" --out-file /path/to/stream-out.txt")
      return
    }

    // Get arguments
    val queryStr = config.getString("query").trim()
    val filePath = config.getString("out-file").trim()

    // Parse arguments
    val query = if (queryStr.length > 0) Option(queryStr.split(" ")) else None
    val outFile = if (filePath.length > 0) Option(new File(filePath)) else None

    // Stream tweets
    val streamer = new TwitterStreamer()
    streamer.stream(query, outFile)
  }

}
