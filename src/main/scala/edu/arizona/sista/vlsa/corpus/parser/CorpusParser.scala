package edu.arizona.sista.vlsa.corpus.parser

import edu.arizona.sista.vlsa.corpus

/** Reads and parses the articles/documents from a corpus, one by one.
  *
  * @author trananh
  */
trait CorpusParser extends Iterable[corpus.Document] {

  /** The name of the corpus */
  var corpusName: String = "Unknown"

  /** Iterator for the corpus */
  def iterator: Iterator[corpus.Document]

  /** Retrieve the next document from the corpus.
    * @return The next corpus document.
    */
  def parseNext(): Option[corpus.Document] = {
    if (this.iterator.hasNext) Option(this.iterator.next()) else None
  }

  /** Close all open streams and clear memory usage */
  def close()

}