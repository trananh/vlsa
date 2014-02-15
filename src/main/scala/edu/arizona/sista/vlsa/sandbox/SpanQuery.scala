package edu.arizona.sista.vlsa.sandbox

import java.io.File
import java.util
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.SlowCompositeReaderWrapper
import org.apache.lucene.index.Term
import org.apache.lucene.index.TermContext
import org.apache.lucene.search.DocIdSetIterator
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.spans.SpanTermQuery
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Bits

/**
 * This class is for demonstration purposes only.  No warranty, guarantee, etc. is implied.
 * <p/>
 * This is not production quality code!
 * 
 * Adapted from: http://searchhub.org/2013/05/09/update-accessing-words-around-a-positional-match-in-lucene-4/
 */
object SpanQuery {

  def main(args: Array[String]) {
    // NOTE: change path to a valid index
    val idx = "/path/to/data/text/indexes/Gigaword-stemmed"

    val idxDir = new File(idx)
    val fieldName = "text"
    val qStr = "chase"

    //Get a searcher
    val searcher = new IndexSearcher(DirectoryReader.open(FSDirectory.open(idxDir)))

    // Do a search using SpanQuery
    val query = new SpanTermQuery(new Term(fieldName, qStr))
    val results = searcher.search(query, 10)
    for (i <- 0 until results.scoreDocs.length) {
      val scoreDoc = results.scoreDocs(i)
      println("Score Doc: " + scoreDoc)
    }
    val reader = searcher.getIndexReader
    //this is not the best way of doing this, but it works for the example.
    // See http://www.slideshare.net/lucenerevolution/is-your-index-reader-really-atomic-or-maybe-slow
    // for higher performance approaches
    val wrapper = SlowCompositeReaderWrapper.wrap(reader)
    val termContexts = new util.HashMap[Term, TermContext]()
    val spans = query.getSpans(wrapper.getContext, new Bits.MatchAllBits(reader.numDocs()), termContexts)
    val window = 5; //get the words within two of the match
    while (spans.next()) {
      val entries = new util.TreeMap[Integer, String]()
      println("Doc: " + spans.doc() + " Start: " + spans.start() + " End: " + spans.end())
      val start = spans.start() - window
      val end = spans.end() + window
      val content = reader.getTermVector(spans.doc(), fieldName)
      val termsEnum = content.iterator(null)
      var term = Option(termsEnum.next())
      while (term.isDefined) {
        //could store the BytesRef here, but String is easier for this example
        val s = new String(term.get.bytes, term.get.offset, term.get.length)
        val positionsEnum = termsEnum.docsAndPositions(null, null)
        if (positionsEnum.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
          var i = 0
          var freq = positionsEnum.freq()
          var position = positionsEnum.nextPosition()
          while (i < freq && position != -1) {
            if (position >= start && position <= end) {
              entries.put(position, s)
            }
            i += 1
            freq = positionsEnum.freq()
            if (i < freq)
              position = positionsEnum.nextPosition()
          }
        }
        term = Option(termsEnum.next())
      }
      println("Entries:" + entries)
    }
  }

}