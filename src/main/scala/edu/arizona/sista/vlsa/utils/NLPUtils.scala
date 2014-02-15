package edu.arizona.sista.vlsa.utils

import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.Processor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import java.io.IOException
import java.io.StringReader
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.util.Version
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

/** Provides some common NLP utilities.
  *
  * @author trananh
  */
object NLPUtils {

  /** CoreNLP processor */
  val proc: Processor = new CoreNLPProcessor(internStrings = false)

  /** Useful subsets of the Penn Treebank Part-of-Speech (POS) tags */
  val POS_JJs = Set("JJ", "JJR", "JJS")
  val POS_VBs = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")
  val POS_NNs = Set("NN", "NNS", "NNP", "NNPS", "PRP", "PRP$", "WP", "WP$")


  /** Analyzes a string using a Lucene analyzer, such as:
    * - StandardAnalyzer: tokenize + stop-words
    * - EnglishAnalyzer: tokenize + stop-words + stemming
    *
    * @param str String to be analyzed.
    * @param analyzer The Lucene analyzer (default to StandardAnalyzer) to use.
    *
    * @return Analyzed string tokens.
    */
  @throws(classOf[IOException])
  def analyzeLucene(str: String,
                    analyzer: Analyzer = new StandardAnalyzer(Version.LUCENE_42)): Array[String] = {
    val result = ArrayBuffer[String]()
    val stream = analyzer.tokenStream(null, new StringReader(str))
    stream.reset()
    while (stream.incrementToken()) {
      result += stream.getAttribute(classOf[CharTermAttribute]).toString
    }
    stream.close()
    result.toArray[String]
  }


  /** Maximum number of words per sentence. Can be used to filter out extremely
    * long sentences that could choke up coreNLP.
    */
  val MAX_SENTENCE_SIZE = 200     // tokens

  /** Parse the string passage into sentences (using CoreNLP tokenizer)
    * and drop all sentences that exceed the specified tokens limit.
    *
    * @param passage String passage to filter.
    * @param sentenceLimit Maximum words (default to MAX_SENTENCE_SIZE).
    *
    * @return Array of sentences that satisfy the limit.
    */
  def dropLongSentences(passage: String,
                        sentenceLimit: Integer = MAX_SENTENCE_SIZE): Array[String] = {
    val sentences = new ListBuffer[String]()
    val doc = proc.mkDocument(passage)
    doc.sentences.foreach(s => {
      if (s.size <= sentenceLimit) sentences += s.words.mkString(" ")
    })
    sentences.toArray[String]
  }

  /** Annotate a document (in place) using Stanford CoreNLP tools.
    *
    * @param doc Document to annotate.
    * @param POS Part-of-speech tag if true.
    * @param lemmatize Lemmatisation if true.
    * @param NER Named-entity recognition if true.
    * @param syntacticParse Syntactic parsing if true.
    * @param chunk Shallow parsing if true.
    * @param semanticRoles Semantic roles labeling if true.
    * @param coref Co-reference resolution if true.
    */
  def annotateCoreNLP(doc: Document,
                      POS: Boolean = false,
                      lemmatize: Boolean = false,
                      NER: Boolean = false,
                      syntacticParse: Boolean = false,
                      chunk: Boolean = false,
                      semanticRoles: Boolean = false,
                      coref: Boolean = false) {
    if (POS) proc.tagPartsOfSpeech(doc)
    if (lemmatize) proc.lemmatize(doc)
    if (NER) proc.recognizeNamedEntities(doc)
    if (syntacticParse) proc.parse(doc)
    if (chunk) proc.chunking(doc)
    if (semanticRoles) proc.labelSemanticRoles(doc)
    if (coref) proc.resolveCoreference(doc)
    doc.clear()
  }

  /** Annotate sentences using Stanford CoreNLP tools.
    *
    * @param sentences Array of un-tokenized sentences.
    * @param POS Part-of-speech tag if true.
    * @param lemmatize Lemmatisation if true.
    * @param NER Named-entity recognition if true.
    * @param syntacticParse Syntactic parsing if true.
    * @param chunk Shallow parsing if true.
    * @param semanticRoles Semantic roles labeling if true.
    * @param coref Co-reference resolution if true.
    *
    * @return Annotated doc.
    */
  def annotateSentences(sentences: Iterable[String],
                        POS: Boolean = false,
                        lemmatize: Boolean = false,
                        NER: Boolean = false,
                        syntacticParse: Boolean = false,
                        chunk: Boolean = false,
                        semanticRoles: Boolean = false,
                        coref: Boolean = false): Document = {
    val doc = proc.mkDocumentFromSentences(sentences)
    annotateCoreNLP(doc, POS, lemmatize, NER, syntacticParse, chunk, semanticRoles, coref)
    doc
  }

  /** Annotate a string using Stanford CoreNLP tools.
    *
    * @param str String to annotate.
    * @param POS Part-of-speech tag if true.
    * @param lemmatize Lemmatisation if true.
    * @param NER Named-entity recognition if true.
    * @param syntacticParse Syntactic parsing if true.
    * @param chunk Shallow parsing if true.
    * @param semanticRoles Semantic roles labeling if true.
    * @param coref Co-reference resolution if true.
    *
    * @return Annotated doc.
    */
  def annotateString(str: String,
                     POS: Boolean = false,
                     lemmatize: Boolean = false,
                     NER: Boolean = false,
                     syntacticParse: Boolean = false,
                     chunk: Boolean = false,
                     semanticRoles: Boolean = false,
                     coref: Boolean = false): Document = {
    val doc = proc.mkDocument(str)
    annotateCoreNLP(doc, POS, lemmatize, NER, syntacticParse, chunk, semanticRoles, coref)
    doc
  }

  /** Return the lemmatize form for each term.
    * @param terms List of terms.
    * @return The lemmatize form of each term.
    */
  def lemmatizeTerms(terms: Array[String]): Array[String] = {
    def getLemma(term: String): String = {
      NLPUtils.annotateString(term, POS = true, lemmatize = true).sentences(0).lemmas.get(0)
    }
    terms.map(w => getLemma(w))
  }

  /** Attempt to find the adjective lemma form for each term.
    * @param terms List of terms.
    * @return The lemmatize form of each term as adjective if possible.
    */
  def lemmatizeTermsAsAdjectives(terms: Array[String]): Array[String] = {
    def getAdjectiveLemma(term: String): String = {
      var doc: Document = null
      val a = if (Set('a','e','i','o','u').contains(term.charAt(0))) "An" else "A"
      doc = NLPUtils.annotateString(a + " " + term + " person.", POS = true, lemmatize = true)
      if (NLPUtils.POS_JJs.contains(doc.sentences(0).tags.get(1)))
        return doc.sentences(0).lemmas.get(1)

      doc = NLPUtils.annotateString("He is very " + term + ".", POS = true, lemmatize = true)
      if (NLPUtils.POS_JJs.contains(doc.sentences(0).tags.get(3)))
        return doc.sentences(0).lemmas.get(3)

      doc = NLPUtils.annotateString(term, POS = true, lemmatize = true)
      if (!NLPUtils.POS_JJs.contains(doc.sentences(0).tags.get(0)))
        if (Constants.DEBUG) println("Couldn't find JJ form for: " + term + " (" + doc.sentences(0).tags.get(0) + ")")
      doc.sentences(0).lemmas.get(0)
    }
    terms.map(w => getAdjectiveLemma(w))
  }

}