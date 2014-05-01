package edu.arizona.sista.vlsa.corpus.index

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.corpus.parser.CorpusParser
import edu.arizona.sista.vlsa.corpus.parser.GigawordTextParser
import java.io.File
import java.io.IOException
import java.util.Date
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

/** Indexer for a corpus using Lucene.
  *
  * @constructor Create an indexer for the corpus.
  * @param parser The corpus parser.
  * @param idxDir Root directory of the index.
  * @param analyzer Lucene analyzer to use, default to StandardAnalyzer
  *                 (e.g., use EnglishAnalyzer to add stemming).
  * @param version Lucene version to use, default to LUCENE_42.
  *
  * @author trananh
  */
class LuceneIndexer(
    val parser: CorpusParser,
    val idxDir: File,
    val analyzer: Analyzer = new StandardAnalyzer(Version.LUCENE_42),
    val version: Version = Version.LUCENE_42) {

  def this(parser: CorpusParser, idxPath: String) = this(parser, new File(idxPath))

  /** Analyzer performs the task of tokenization plus other things */
  val iwc: IndexWriterConfig = new IndexWriterConfig(version, analyzer)

  /** Index the corpus using Lucene (v4.2).
    * @param append Append to the existing index directory if True, else create new.
    * @param ramBufferSize Size of the RAM buffer to use while indexing.
    * @param runNLP Include CoreNLP results in the index.
    * @param storePostings Store postings information for the text if true.
    * @param storeTermVec Store the text into Term Vectors if set to true.
    */
  @throws(classOf[IOException])
  def index(append: Boolean = false, ramBufferSize: Double = 1024.0,
            runNLP: Boolean = false, storePostings: Boolean = false,
            storeTermVec: Boolean = false) {

    val start = new Date()
    println("Indexing started on: " + parser.corpusName)

    if (runNLP) println("  + with NLP")
    if (storePostings) println("  + with postings")
    if (storeTermVec) println("  + with term vectors")

    // Index to directory (not RAM)
    val dir: Directory = FSDirectory.open(idxDir)

    // Instantiate index writer
    iwc.setOpenMode(if (append) OpenMode.CREATE_OR_APPEND else OpenMode.CREATE)
    iwc.setRAMBufferSizeMB(ramBufferSize)
    val indexWriter = new IndexWriter(dir, iwc)

    // Iterate through and add each doc to the indexer
    var count = 0
    var next = parser.parseNext()
    while (next.isDefined) {
      // Index doc
      if (runNLP)
        next.get.processNLP()
      indexWriter.addDocument(next.get.toLuceneDocument(
        storePostings = storePostings,
        storeTermVec = storeTermVec))

      // Book keeping
      count += 1
      if (count % 10000 == 0)
        println("Docs completed: " + count)

      // Get next one
      next = parser.parseNext()
    }

    // Forces merging of segments (optimize search process at the cost of longer index)
    indexWriter.forceMerge(1)

    // Close the writer and directory
    indexWriter.close()
    dir.close()

    println("Indexing completed. Total time: " + (new Date().getTime - start.getTime) + " milliseconds")
  }

}

object RunLuceneIndexer {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new Switch("run-nlp")
      .setShortFlag('n').setLongFlag("run-nlp"))
    jsap.registerParameter(new Switch("stem")
      .setShortFlag('s').setLongFlag("stem"))
    jsap.registerParameter(new Switch("postings")
      .setShortFlag('p').setLongFlag("postings"))
    jsap.registerParameter(new Switch("term-vector")
      .setShortFlag('t').setLongFlag("term-vector"))
    jsap.registerParameter(new FlaggedOption("corpus")
      .setDefault("").setRequired(true).setLongFlag("corpus"))
    jsap.registerParameter(new FlaggedOption("corpus-dir")
      .setDefault("").setRequired(true).setLongFlag("corpus-dir"))
    jsap.registerParameter(new FlaggedOption("index-dir")
      .setDefault("").setRequired(true).setLongFlag("index-dir"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("corpus"))
      || "".equals(config.getString("corpus-dir"))
      || "".equals(config.getString("index-dir"))) {
      print("index-corpus ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("index-corpus -s --corpus gigaword " +
        "--corpus-dir /path/to/text/corpora/Gigaword/data " +
        "--index-dir /path/to/text/indexes/Gigaword-stemmed")
      return
    }

    // Get arguments
    val runNLP = config.getBoolean("run-nlp")
    val stem = config.getBoolean("stem")
    val storeTermVec = config.getBoolean("term-vector")
    val storePostings = config.getBoolean("postings")
    val corpus = config.getString("corpus").trim()
    val corpusDir = config.getString("corpus-dir").trim()
    val indexDir = config.getString("index-dir").trim()

    // Try to find the right parser for the corpus
    var parser: Option[CorpusParser] = None
    corpus.toLowerCase match {
      case "gigaword" => parser = Option(new GigawordTextParser(corpusDir))
      case _ => throw new Exception("Unrecognized corpus!")
    }

    // Index corpus
    if (parser.isDefined) {
      val indexer = new LuceneIndexer(parser.get, new File(indexDir),
        analyzer = if (stem) new EnglishAnalyzer(Version.LUCENE_42)
        else new StandardAnalyzer(Version.LUCENE_42))

      indexer.index(ramBufferSize = 2048.0,
        runNLP = runNLP,
        storePostings = storePostings,
        storeTermVec = storeTermVec)
    } else {
      println("Unrecognized corpus!  Try: 'gigaword'")
    }

    parser.get.close()
  }
  
}