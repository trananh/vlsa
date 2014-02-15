package edu.arizona.sista.vlsa.sandbox

import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.corpus.parser.GigawordTextParser
import edu.arizona.sista.vlsa.search.lucene.IndexSearcher
import edu.arizona.sista.vlsa.search.lucene.QueryBuilder
import edu.arizona.sista.vlsa.utils.PrinterUtils
import java.io.File
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version
import org.tartarus.snowball.ext.PorterStemmer

object Prototype {

  def main(args: Array[String]) {
    // Call any of the functions below to test them
    val corpusPath = "/path/to/text/corpora/Gigaword/data"
    val index = "/path/to/data/text/indexes/Gigaword-stemmed"

  }

  def annotateCorpus(corpusPath: String) {
    // Iterate through gigaword corpus and annotate an example story article
    val corpus = new GigawordTextParser(corpusPath)
    var corpusDoc = corpus.iterator.next()
    while (corpus.iterator.hasNext && !corpusDoc.attributes.get("type").equals("story")
      && corpusDoc.text.length() < 2500) {
      corpusDoc = corpus.iterator.next()
    }
    val proc: Processor = new CoreNLPProcessor()
    val doc = proc.annotate(corpusDoc.text)
    println(PrinterUtils.printNLPDoc(doc))
    corpus.close()
  }

  def stemmer() {
    // Using porter stemmer
    val stemmer = new PorterStemmer()
    stemmer.setCurrent("chases")
    stemmer.stem()
    println(stemmer.getCurrent)
  }

  def highlighters(index: String) {
    // Test performance of highlighters
    val searcher = new IndexSearcher(new File(index), "text", analyzer = new EnglishAnalyzer(Version.LUCENE_42))
    val query = QueryBuilder.mkQuery("\"chase\" AND \"scare\"", "text", searcher.analyzer, searcher.version)
    searcher.search(query)
    println("\n*******\n")
    searcher.highlightStandard(query, "text")
    println("\n*******\n")
    searcher.highlightPostings(query, "text")
  }

}