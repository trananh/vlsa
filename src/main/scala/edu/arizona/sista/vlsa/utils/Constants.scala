package edu.arizona.sista.vlsa.utils

import java.io.{FileInputStream, File}
import java.util.Properties
import scala.Option

/** Place to hold all global (i.e., system level) constants and properties.
  * This class will try to look for a properties file named "config.properties" and load
  * the properties if the file exists.
  *
  * @author trananh
  */
object Constants {

  /** Debug flag */
  var DEBUG = false


  /** Random number generator seed */
  val RANDOM_SEED = 0


  /** VLSA project root */
  val VLSA_ROOT = System.getProperty("user.dir") + "/research/src/main/scala/edu/arizona/sista/vlsa"


  /** Load configuration properties if file exists */
  private val propertiesFile = new File(VLSA_ROOT + "/config.properties")
  private val config = new Properties()
  if (propertiesFile.exists()) {
    config.load(new FileInputStream(propertiesFile))
  }


  /** Google search properties */
  object GoogleProperties {
    var apiKey = "UNKNOWN"
    var cseID = "UNKNOWN"

    if (config.containsKey("google.apiKey")) apiKey = config.getProperty("google.apiKey")
    if (config.containsKey("google.cseID")) cseID = config.getProperty("google.cseID")
  }


  /** WordNet's properties. */
  object WordNetProperties {
    var databaseDir = "UNKNOWN"

    if (Option(System.getProperty("wordnet.database.dir")).isDefined)
      databaseDir = System.getProperty("wordnet.database.dir")
    else if (config.containsKey("wordnet.databaseDir"))
      databaseDir = config.getProperty("wordnet.databaseDir")
  }


  /** Twitter properties. */
  object TwitterProperties {
    var oauthConsumerKey = "UNKNOWN"
    var oauthConsumerSecret = "UNKNOWN"
    var oauthAccessToken = "UNKNOWN"
    var oauthAccessTokenSecret = "UNKNOWN"

    if (config.containsKey("twitter.oauth.consumerKey"))
      oauthConsumerKey = config.getProperty("twitter.oauth.consumerKey")
    if (config.containsKey("twitter.oauth.consumerSecret"))
      oauthConsumerSecret = config.getProperty("twitter.oauth.consumerSecret")
    if (config.containsKey("twitter.oauth.accessToken"))
      oauthAccessToken = config.getProperty("twitter.oauth.accessToken")
    if (config.containsKey("twitter.oauth.accessTokenSecret"))
      oauthAccessTokenSecret = config.getProperty("twitter.oauth.accessTokenSecret")
  }


  /** Faroo search properties */
  object FarooProperties {
    var apiKey = "UNKNOWN"

    if (config.containsKey("faroo.apiKey")) apiKey = config.getProperty("faroo.apiKey")
  }


  /** Bing search properties */
  object BingProperties {
    var accountKey = "UNKNOWN"

    if (config.containsKey("bing.accountKey")) accountKey = config.getProperty("bing.accountKey")
  }

}
