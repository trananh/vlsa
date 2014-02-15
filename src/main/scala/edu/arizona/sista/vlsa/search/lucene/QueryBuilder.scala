package edu.arizona.sista.vlsa.search.lucene

import edu.arizona.sista.vlsa.utils.NLPUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.Term
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.Query
import org.apache.lucene.search.TermQuery
import org.apache.lucene.util.Version

/** Builder for common query patterns in Lucene.
  * 
  * @author trananh
  */
object QueryBuilder {

  /** Make a Lucene query from a query string.
    * @param queryStr The query string.
    * @param fieldName The default field for query.
    * @param analyzer Lucene analyzer (default to StandardAnalyzer).
    * @param version Lucene version.
    * @return A Lucene query object.
    */
  def mkQuery(queryStr: String,
              fieldName: String,
              analyzer: Analyzer = new StandardAnalyzer(Version.LUCENE_42),
              version: Version = Version.LUCENE_42): Query = {
    val parser = new QueryParser(version, fieldName, analyzer)
    parser.parse(queryStr)
  }

  /** Make a term query for the given term.
    * @param term The term to query.
    * @param fieldName The default field for query.
    * @param analyzer Lucene analyzer (default to StandardAnalyzer).
    * @return A Lucene query object.
    */
  def mkTermQuery(term: String, fieldName: String,
                  analyzer: Analyzer = new StandardAnalyzer(Version.LUCENE_42)): Query = {
    val analyzed = NLPUtils.analyzeLucene(term, analyzer = analyzer)
    new TermQuery(new Term(fieldName, analyzed.mkString("-")))
  }
}