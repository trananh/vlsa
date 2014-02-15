package edu.arizona.sista.vlsa.demos

import edu.smu.tspell.wordnet.NounSynset
import edu.smu.tspell.wordnet.Synset
import edu.smu.tspell.wordnet.SynsetType
import edu.smu.tspell.wordnet.WordNetDatabase

object WordNetDemo {

  def main(args: Array[String]) {

    // Set WordNet's dictionary installation directory
    if (!Option(System.getProperty("wordnet.database.dir")).isDefined)
      System.setProperty("wordnet.database.dir", "/usr/local/WordNet-3.0/dict")

    // Demo
    var nounSynset: NounSynset = null
    var hyponyms: Array[NounSynset] = null

    val database = WordNetDatabase.getFileInstance
    val synsets: Array[Synset] = database.getSynsets("fly", SynsetType.NOUN)
    for (i <- 0 until synsets.length) {
      nounSynset = synsets(i).asInstanceOf[NounSynset]
      hyponyms = nounSynset.getHyponyms
      println(nounSynset.getWordForms()(0) +
        ": " + nounSynset.getDefinition + ") has " + hyponyms.length + " hyponyms")
    }

  }
}