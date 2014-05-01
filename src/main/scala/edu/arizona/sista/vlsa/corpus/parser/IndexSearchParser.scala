package edu.arizona.sista.vlsa.corpus.parser

import edu.arizona.sista.vlsa.corpus
import edu.arizona.sista.vlsa.search.lucene.IndexSearcher
import java.io.IOException
import org.apache.lucene.search.ScoreDoc

/** A simple parser that reads documents from an index search and returns the documents iteratively.
  *
  * @constructor Creates a new lucene index search parser for a list of lucene search results.
  *
  * @author trananh
  */
class IndexSearchParser(searcher: IndexSearcher, topHits: Iterator[ScoreDoc]) extends CorpusParser {

  /** The name of the corpus */
  this.corpusName = "Lucene Documents"

  /** Iterator for the corpus */
  def iterator = new Iterator[corpus.Document] {

    /** Return true if there is another document in the corpus. */
    @throws(classOf[IOException])
    def hasNext: Boolean = {
      topHits.hasNext
    }

    /** Get the next document in the corpus. */
    @throws(classOf[IOException])
    def next(): corpus.Document = {
      val doc = corpus.Document.fromLuceneDocument(searcher.getHit(topHits.next.doc))
      doc.corpus = corpusName
      doc
    }
  }

  /** Close all open streams and clear memory usage */
  def close() = { /* Nothing to close */ }

}