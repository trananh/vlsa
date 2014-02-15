package edu.arizona.sista.vlsa.corpus

import edu.arizona.sista.processors
import edu.arizona.sista.processors.DocumentSerializer
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.utils.PrinterUtils
import org.apache.lucene
import org.apache.lucene.document.{StoredField, StringField, Field, FieldType}
import org.apache.lucene.index.FieldInfo.IndexOptions
import org.apache.lucene.index.IndexableField
import scala.collection.mutable

/** A simple abstraction of a document that came from some corpus.
  *
  * @constructor Create a document with some text, corpus, and optional attributes.
  * @param text The document's main text contents.
  * @param corpus Identification of the source corpus containing the document.
  * @param attributes Optional attributes stored as [K,V] pairs.
  *
  * @author trananh
  */
class Document(
    val text: String,
    val corpus: String = "",
    var attributes: mutable.Map[String, Any] = mutable.Map.empty[String, Any]) {

  /** Process the document through the Stanford CoreNLP toolbox.
    * The NLP results are added to the attributes map under the
    * key "NLP", and also returned from the function.
    *
    * @return A CoreNLP document from the text of the document.
    */
  def processNLP(): processors.Document = {
    val nlpDoc = Document.nlpProcessor.annotate(this.text)
    attributes("NLP") = nlpDoc
    nlpDoc
  }

  /** Create a Lucene document for indexing.
    * @param storePostings Store postings information for the text if true.
    * @param storeTermVec Store the text into Term Vectors if set to true.
    * @return A lucene document containing the information of this document.
    */
  def toLuceneDocument(storePostings: Boolean = false,
                       storeTermVec: Boolean = false): lucene.document.Document = {

    // Create a lucene doc for indexing
    val doc = new lucene.document.Document()

    // Add corpus name (indexed, but not tokenized)
    doc.add(new StringField("corpus", this.corpus, Field.Store.YES))

    // Add the attributes (stored, but not indexed or analyzed)
    // We'll only store string fields for now.
    var hasNLP = false
    for ((k, v) <- attributes) {
      v match {
        case v: String =>
          doc.add(new StoredField(k, v))
        case v: processors.Document => {
          hasNLP = true
          doc.add(new StoredField(k, Document.nlpDocSerializer.save(doc = v)))
        }
        case _ => null
      }
    }

    // Add the text contents (indexed, tokenized, and stored).
    val fType = new FieldType()
    if (storeTermVec) {
      fType.setStoreTermVectors(true)
      fType.setStoreTermVectorOffsets(true)
      fType.setStoreTermVectorPositions(true)
    }
    if (storePostings) {
      fType.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS)
    }
    fType.setIndexed(true)
    fType.setTokenized(true)
    fType.setStored(true)
    doc.add(new Field("text", this.text, fType))

    doc
  }

  /** Return a string description of the document. */
  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("corpus:\n======\n" + corpus + "\n\n")
    if (text.length > 0)
      sb.append("text:\n======\n" + text + "\n\n")
    for ((k, v) <- attributes) {
      v match {
        case v: String =>
          sb.append(k + ":\n======\n" + v + "\n\n")
        case v: processors.Document =>
          sb.append(k + ":\n======\n" + PrinterUtils.printNLPDoc(v) + "\n\n")
        case _ => null
      }
    }
    sb.toString()
  }

}

object Document {

  /** Serializer for CoreNLP document */
  private val nlpDocSerializer = new DocumentSerializer()

  /** Processor for CoreNLP */
  private val nlpProcessor = new CoreNLPProcessor()

  /** Create a new corpus document from the queried lucene document.
    * @param doc A Lucene document.
    * @return A corpus document.
    */
  def fromLuceneDocument(doc: lucene.document.Document): Document = {
    var text = ""
    var corpus = ""
    val attributes: mutable.Map[String, Any] = mutable.Map.empty[String, Any]

    // Iterate through each field
    for (f <- doc.getFields.toArray) {
      val field = f.asInstanceOf[IndexableField]
      val tag = field.name()
      tag.toLowerCase match {
        case "corpus" => corpus = doc.get(tag)
        case "text" => text = doc.get(tag)
        case "nlp" => attributes(tag) = nlpDocSerializer.load(doc.get(tag))
        case _ => attributes(tag) = doc.get(tag)
      }
    }

    if (text.length <= 0 && attributes.get("NLP").isDefined) {
      val sb = new StringBuilder()
      val doc = attributes.get("NLP").get.asInstanceOf[processors.Document]
      for (sentence <- doc.sentences) {
        sb.append(sentence.words.mkString(" ") + "\n")
      }
      text = sb.toString()
    }

    new Document(text, corpus, attributes)
  }

}