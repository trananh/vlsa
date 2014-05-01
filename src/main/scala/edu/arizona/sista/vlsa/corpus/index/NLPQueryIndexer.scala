package edu.arizona.sista.vlsa.corpus.index

import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.corpus.parser.IndexSearchParser
import edu.arizona.sista.vlsa.search.lucene.IndexSearcher
import edu.arizona.sista.vlsa.search.lucene.QueryBuilder
import java.io.File
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version

/** A program for creating a new Lucene index using documents from an existing index (via a query)
  * and adding NLP annotation to them.  The steps are:
  *  - query an existing index
  *  - for each resulting article, annotate NLP
  *  - create a new index.
  *
  * @author trananh
  */
object NLPQueryIndexer {

  /** A program for creating a new Lucene index using documents from an existing index (via a query)
    * and adding NLP annotation to them.  The steps are:
    *  - query an existing index
    *  - for each resulting article, annotate NLP
    *  - create a new index.
    *
    * @param args Command-line arguments.
    */
  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("index-dir")
      .setDefault("").setRequired(true).setLongFlag("index-dir"))
    jsap.registerParameter(new FlaggedOption("out-dir")
      .setDefault("").setRequired(true).setLongFlag("out-dir"))
    jsap.registerParameter(new FlaggedOption("query")
      .setDefault("").setRequired(true).setLongFlag("query"))
    jsap.registerParameter(new FlaggedOption("field")
      .setDefault("text").setRequired(false).setLongFlag("field"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("index-dir"))
      || "".equals(config.getString("out-dir"))
      || "".equals(config.getString("query"))) {
      print("query-indexer ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("query-indexer --index-dir /path/to/text/indexes/Gigaword-stemmed" +
        " --out-dir /path/to/text/indexes/QueryGigaword" +
        " --query \"chase\"" +
        " --field \"text\"")
      return
    }

    // Get arguments
    val indexDir = config.getString("index-dir").trim()
    val fieldName = config.getString("index-dir").trim()
    val queryStr = config.getString("query").trim()
    val outDir = config.getString("out-dir").trim()

    val searcher = new IndexSearcher(new File(indexDir), fieldName, analyzer = new EnglishAnalyzer(Version.LUCENE_42))

    val query = QueryBuilder.mkQuery(queryStr, fieldName, analyzer = searcher.analyzer, version = searcher.version)
    val hits = searcher.search(query)

    val parser = new IndexSearchParser(searcher, hits.iterator)
    parser.corpusName = "Annotated Gigaword Corpus (" + queryStr + ")"

    val indexer = new LuceneIndexer(parser, new File(outDir), analyzer = searcher.analyzer)
    indexer.index(ramBufferSize = 2048.0, runNLP = true, storePostings = true, storeTermVec = true)

  }

}