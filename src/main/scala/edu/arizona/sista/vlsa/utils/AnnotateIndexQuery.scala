package edu.arizona.sista.vlsa.utils

import edu.arizona.sista.processors.DocumentSerializer
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.vlsa.corpus
import edu.arizona.sista.vlsa.corpus.parser.IndexSearchParser
import edu.arizona.sista.vlsa.search.lucene.{QueryBuilder, IndexSearcher}
import java.io.File
import java.util.Calendar
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version

/** Query an index and run CoreNLP annotation on each resulting document.
  * The results from each step (index query & NLP annotation) are written to
  * output directories in (XML & serialized format, respectively).
  *
  * @author trananh
  */
object AnnotateIndexQuery {

  /** Number of bytes in a MB */
  val KB = 1024
  val MB = 1024 * 1024
  val MAX_SIZE = 1 * MB                         /* Max file size for NLP processing, 1MB */
  val MIN_SIZE = 0 * KB                         /* Min file size for NLP processing, 0 */

  /* NLP processor and serializer */
  private val processor = new CoreNLPProcessor(internStrings = false)
  private val serializer = new DocumentSerializer()

  /** Perform an index query and save all resulting query docs to .txt files
    * (although the format is XML).
    *
    * @param searcher An index searcher.
    * @param queryStr The query string (e.g. "chase").
    * @param fieldName The field name for the query (defaults to "text").
    * @param outDir Path to the output directory for the XMLs.
    * @param overwrite Overwrite any existing files if True (defaults to false).
    */
  def queryToXMLFile(searcher: IndexSearcher, queryStr: String, fieldName: String = "text",
                     outDir: String, overwrite: Boolean = false) {

    // Perform index query
    val query = QueryBuilder.mkQuery(queryStr, fieldName, analyzer = searcher.analyzer, version = searcher.version)
    val hits = searcher.search(query)

    // Iterate through the hits and write them to files
    val parser = new IndexSearchParser(searcher, hits.iterator)
    parser.corpusName = "Queried Corpus (" + queryStr + ")"
    var next = parser.parseNext()
    while (next.isDefined) {
      val id = next.get.attributes.get("id").get.asInstanceOf[String]
      val file = new File(outDir + "/" + id + ".txt")
      if (overwrite || !file.exists()) {
        val writer = new java.io.FileWriter(file)
        writer.write(next.get.toXML)
        writer.close()
      }
      next = parser.parseNext()
    }
  }

  /** Filter for potentially hazardous documents, such as documents that are too large in size or contain
    * malformed (long) sentences.  These documents could choke and hang the NLP annotators.
    *
    * @param document The corpus document to verify.
    * @param maxSentenceSize Return false if any sentence in the document exceeds this value
    *                        (defaults to [[edu.arizona.sista.vlsa.utils.NLPUtils.MAX_SENTENCE_SIZE]]).
    * @param minSize Return false if the number of characters in the document is less than or equal to this value
    *                (defaults to 0).
    * @param maxSize Return false if the number of characters in the document exceeds this value
    *                (defaults to 1MB).
    *
    * @return True if the document passes and should be processed.
    */
  def filterDoc(document: corpus.Document, maxSentenceSize: Int = NLPUtils.MAX_SENTENCE_SIZE,
                        minSize: Int = MIN_SIZE, maxSize: Int = MAX_SIZE): Boolean = {

    val now = Calendar.getInstance().getTime().toString
    val id = document.attributes.get("id").get.asInstanceOf[String]

    /* Impose some file-size filtering */
    if (document.text.length <= minSize) {
      if (Constants.DEBUG) println("[" + now + "] SKIPPING: " + id + " below or equal to " + minSize + " characters.")
      return false
    }
    else if (document.text.length > maxSize) {
      if (Constants.DEBUG) println("[" + now + "] SKIPPING: " + id + " exceeds " + maxSize + " characters.")
      return false
    }

    // We also cannot handle long sentences
    val doc = processor.mkDocument(document.text)
    val longSentences = doc.sentences.map(s => s.size).sorted
    if (longSentences.last > maxSentenceSize) {
      if (Constants.DEBUG)
        println("[" + now + "] SKIPPING: " + id + " contains at least one long sentence exceeding " +
          maxSentenceSize + " tokens.")
      return false
    }

    // Document passes
    if (Constants.DEBUG)
      println("[" + now + "] PROCESSING: " + id + " (total-chars = " + document.text.length +
        "; max-sent = " + longSentences.last + ").")

    return true
  }

  /** Read corpus documents from the given directory and run CoreNLP annotation
    * on each document.  Serialize the output to the specified directory.
    * 
    * @param dataDir Path to directory containing the documents (expecting .txt files).
    * @param outDir Path to output directory for serialized NLP annotations.
    * @param overwrite Overwrite any existing files if True (defaults to false).
    * @param maxSentenceSize Skip documents with any sentence exceeding this value in length
    *                        (defaults to [[edu.arizona.sista.vlsa.utils.NLPUtils.MAX_SENTENCE_SIZE]]).
    * @param minSize Skip any document containing less than or equal to minSize number of characters (defaults to 0).
    * @param maxSize Skip any document exceeding maxSize number of characters (defaults to 1MB).
    */
  def annotateNLP(dataDir: String, outDir: String, overwrite: Boolean = false,
                  maxSentenceSize: Int = NLPUtils.MAX_SENTENCE_SIZE,
                  minSize: Int = MIN_SIZE, maxSize: Int = MAX_SIZE) {
    // Retrieve the data directory
    val dataDirFile = new File(dataDir)

    // Iterate over each file and process it with CoreNLP
    dataDirFile.listFiles().foreach(file => {
      // Use the same filename for the output
      val filename = file.getName

      // We only expect txt files
      if (file.isFile && filename.endsWith(".txt")) {

        val nlpFile = new File(outDir + "/" + filename)
        if (overwrite || !nlpFile.exists()) {

          // Read the document XML
          val source = scala.io.Source.fromFile(file)
          val xml = source.mkString
          source.close()

          // Load the XML to a doc
          val doc = corpus.Document.fromXML(xml)

          // Annotate NLP and write to file
          try {
            if (filterDoc(doc, maxSentenceSize, minSize, maxSize)) {
              // Process NLP
              val nlpDoc = doc.processNLP()
              val str = serializer.save(nlpDoc)

              // Write to file
              val writer = new java.io.FileWriter(nlpFile)
              writer.write(str)
              writer.close()
            }
          } catch {
            case e: Exception =>
              println("ERROR: " + file.getName + "\n" + e.getMessage + "\n" + e.getStackTrace.toString)
          }
        }
        
      }

    })

  }

  /** Demo: Query an index and run CoreNLP annotation on each resulting document.
    */
  def main(args: Array[String]) {

    // IMPORTANT: The paths below are hardcoded for now.Change them accordingly before running.
    val queryStr = "hug"
    val fieldName = "text"
    val indexDir = "/home/trananh/Workspace/data/text/indexes/Gigaword-stemmed"
    val queryDir = "/home/trananh/Workspace/data/text/corpora/Gigaword-hug"
    val nlpDir = "/home/trananh/Workspace/data/text/corpora/Gigaword-hug-nlp"

    println("\n\nQuerying index and saving results to file ...")
    val searcher = new IndexSearcher(new File(indexDir), fieldName, analyzer = new EnglishAnalyzer(Version.LUCENE_42))
    queryToXMLFile(searcher, queryStr, fieldName, queryDir, overwrite = false)

    println("\n\nProcessing NLP and saving results to file ...")
    annotateNLP(queryDir, nlpDir, overwrite = false)

  }

}
