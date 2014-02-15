package edu.arizona.sista.vlsa.models.text.word

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.utils.Constants
import edu.smu.tspell.wordnet._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/** Different models for extracting synsets from WordNet.
  * @param database Path to WordNet's dictionary installation directory
  * @author trananh
  */
class WordNetSynset(database: String = Constants.WordNetProperties.databaseDir) {

  /** Set WordNet's dictionary installation directory */
  System.setProperty("wordnet.database.dir", database)

  /** Find all adjective synsets for the given word and combine them into a single
    * adjective synonym set.
    * @param word The word to look for.
    * @param similar Include synsets, if any, with similar meaning for each synonym.
    * @param related Include synsets representing related ("see also") concepts for each synonym.
    * @return Set of all adjective synonyms for the given word.
    */
  private def findSynsets(word: String,
                         similar: Boolean = false,
                         related: Boolean = false): Set[String] = {
    val database = WordNetDatabase.getFileInstance
    val results = new ListBuffer[String]()
    var synsets = database.getSynsets(word, SynsetType.ADJECTIVE).map(_.asInstanceOf[AdjectiveSynset])
    if (synsets.length == 0)
      synsets = database.getSynsets(word, SynsetType.ADJECTIVE_SATELLITE).map(_.asInstanceOf[AdjectiveSatelliteSynset])
    synsets.foreach(set => {
      results.appendAll(set.getWordForms)
      if (set.isInstanceOf[AdjectiveSatelliteSynset])
        results.appendAll(set.asInstanceOf[AdjectiveSatelliteSynset].getHeadSynset.getWordForms)
      if (similar) set.getSimilar().foreach(s => results.appendAll(s.getWordForms))
      if (related) set.getRelated().foreach(s => results.appendAll(s.getWordForms))
    })
    results.toSet[String]
  }

  /** Recursively find all adjective synsets for the given word and combine them into a single
    * adjective synonym set.  The algorithm recursively searches for synsets of synsets until
    * all possible synsets reachable from the given word are recovered.
    * @param word The word to look for.
    * @param similar Include synsets, if any, with similar meaning for each synonym.
    * @param related Include synsets representing related ("see also") concepts for each synonym.
    * @return Set of all adjective synonyms for the given word.
    */
  private def findSynsetsRecursive(word: String,
                                  similar: Boolean = false,
                                  related: Boolean = false): Set[String] = {
    def addSynsetsRecursive(allSets: mutable.HashMap[Int, Array[String]], synsets: Array[AdjectiveSynset]) {
      synsets.foreach(set => {
        if (!allSets.contains(set.hashCode())) {
          allSets.put(set.hashCode(), set.getWordForms)
          if (set.isInstanceOf[AdjectiveSatelliteSynset]) {
            val headSet = set.asInstanceOf[AdjectiveSatelliteSynset].getHeadSynset
            if (!allSets.contains(headSet.hashCode()))
              allSets.put(headSet.hashCode(), headSet.getWordForms)
          }
          if (similar) addSynsetsRecursive(allSets, set.getSimilar())
          if (related) addSynsetsRecursive(allSets, set.getRelated())
        }
      })
    }
    val database = WordNetDatabase.getFileInstance
    val allSets = new mutable.HashMap[Int, Array[String]]()
    var seedSets = database.getSynsets(word, SynsetType.ADJECTIVE).map(_.asInstanceOf[AdjectiveSynset])
    if (seedSets.length == 0)
      seedSets = database.getSynsets(word, SynsetType.ADJECTIVE_SATELLITE).map(_.asInstanceOf[AdjectiveSatelliteSynset])
    addSynsetsRecursive(allSets, seedSets)

    val results = new ListBuffer[String]()
    allSets.keys.foreach(k => results.appendAll(allSets.get(k).get))
    results.toSet[String]
  }

  /** Find all adjective synsets for the given word and combine them into a single
    * adjective synonym set.
    * @param word The word to look for.
    * @param recursive Recursively find all connecting synsets (can lead to a lot of loosely
    *                  connected words).
    * @param similar Include synsets, if any, with similar meaning for each synonym.
    * @param related Include synsets representing related ("see also") concepts for each synonym.
    * @return Set of all adjective synonyms for the given word.
    */
  def getAdjectiveSynset(word: String,
                         recursive: Boolean = false,
                         similar: Boolean = false,
                         related: Boolean = false): Set[String] = {
    if (recursive) findSynsetsRecursive(word, similar, related)
    else findSynsets(word, similar, related)
  }

  /** Expand the given set of words (e.g., a dictionary of terms) by adding their synsets
    * to yield a larger, more general, set of words.  The algorithm follows:
    * - for each word in the dictionary, add its synsets to the dictionary
    * - repeat the previous step until the dictionary ceases to grow
    * - return the new dictionary
    * @param wordsSet The original set of words
    * @param recursive Recursively find all connecting synsets (can lead to a lot of loosely
    *                  connected words).
    * @param similar Include synsets, if any, with similar meaning for each synonym.
    * @param related Include synsets representing related ("see also") concepts for each synonym.
    * @return Larger set of words grown by adding the synsets of each term from the original set.
    */
  def expandWordsSet(wordsSet: Set[String],
                         recursive: Boolean = false,
                         similar: Boolean = false,
                         related: Boolean = false): Set[String] = {
    var newSynonyms = ListBuffer[String]()
    var synset = wordsSet
    var prevLength = 0
    while (synset.size > prevLength) {
      if (Constants.DEBUG) println("Vocab size: " + synset.size)
      prevLength = synset.size
      newSynonyms = new ListBuffer[String]()
      synset.foreach(word => {
        newSynonyms.appendAll(getAdjectiveSynset(word, recursive, similar, related).map(_.toLowerCase()))
      })
      synset = synset | newSynonyms.toSet[String]
    }
    synset
  }
}


object RunWordNetSynset {

  /** Expand a list of words by adding their synsets from WordNet. */
  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("seed-file")
      .setDefault("").setRequired(true).setLongFlag("seed-file"))
    jsap.registerParameter(new FlaggedOption("out-file")
      .setDefault("").setRequired(true).setLongFlag("out-file"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("seed-file"))
      || "".equals(config.getString("out-file"))) {
      print("wordnet-synset ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("wordnet-synset --seed-file /path/to/data/text/dictionaries/mental-states.txt --out-file ./out.txt")
      return
    }

    // Get arguments
    val seedFile = config.getString("seed-file").trim()
    val outFile = config.getString("out-file").trim()

    // Read in initial set of words
    val seedSet = new ListBuffer[String]()
    for(line <- Source.fromFile(seedFile).getLines()) {
      seedSet.append(line.trim().toLowerCase())
    }

    // Use synset model to expand the words set
    val synsetModel = new WordNetSynset()
    val synset = synsetModel.expandWordsSet(seedSet.toSet[String])

    // Sort the new set alphabetically
    val sortedSet = synset.toList.sortWith(_.toLowerCase < _.toLowerCase)
    if (Constants.DEBUG) println("\n" + sortedSet.mkString(","))

    // Write to file
    val out = new java.io.FileWriter(outFile)
    out.write(sortedSet.mkString("\n"))
    out.close
  }

}
