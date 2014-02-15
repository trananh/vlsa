package edu.arizona.sista.vlsa.utils

import edu.arizona.sista.processors
import edu.arizona.sista.processors.struct.DirectedGraphEdgeIterator

/** Provides some common Printer utilities.
  * 
  * @author trananh
  */
object PrinterUtils {

  def printNLPDoc(doc: processors.Document): String = {
    val sb = new StringBuilder()

    // let's print the sentence-level annotations
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      sb.append("Sentence #" + sentenceCount + ":\n")
      sb.append("Tokens: " + sentence.words.mkString(" ") + "\n")
      sb.append("Start character offsets: " + sentence.startOffsets.mkString(" ") + "\n")
      sb.append("End character offsets: " + sentence.endOffsets.mkString(" ") + "\n")

      // these annotations are optional, so they are stored using Option objects, hence the foreach statement
      sentence.lemmas.foreach(lemmas => sb.append("Lemmas: " + lemmas.mkString(" ") + "\n"))
      sentence.tags.foreach(tags => sb.append("POS tags: " + tags.mkString(" ") + "\n"))
      sentence.entities.foreach(entities => sb.append("Named entities: " + entities.mkString(" ") + "\n"))
      sentence.norms.foreach(norms => sb.append("Normalized entities: " + norms.mkString(" ") + "\n"))
      sentence.dependencies.foreach(dependencies => {
        sb.append("Syntactic dependencies:\n")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          sb.append(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3 + "\n")
        }
      })
      sentence.syntacticTree.foreach(tree => {
        sb.append("Constituent tree: " + tree + "\n")
        // see the edu.arizona.sista.utils.Tree class for more information
        // on syntactic trees, including access to head phrases/words
      })

      sentenceCount += 1
      sb.append("\n\n")
    }

    // let's print the co-reference chains
    doc.coreferenceChains.foreach(chains => {
      for (chain <- chains.getChains) {
        sb.append("Found one co-reference chain containing the following mentions:\n")
        for (mention <- chain) {
          // note that all these offsets start at 0 too
          sb.append("\tsentenceIndex:" + mention.sentenceIndex +
            " headIndex:" + mention.headIndex +
            " startTokenOffset:" + mention.startOffset +
            " endTokenOffset:" + mention.endOffset +
            " text: " + doc.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]") +
            "\n")
        }
      }
    })

    sb.toString()
  }
  
}