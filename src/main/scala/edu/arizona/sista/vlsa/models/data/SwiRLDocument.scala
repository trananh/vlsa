package edu.arizona.sista.vlsa.models.data

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** A SwiRL's parsed document.
  *
  * @constructor Create a document containing parsed sentences.
  * @param sentences List of parsed sentences.
  *
  * @author trananh
  */
class SwiRLDocument(val sentences: List[SwiRLSentence]) {
  override def toString: String = {
    val os = new StringBuilder()
    sentences.foreach(s => os.append(s.toString + "\n"))
    os.toString
  }
}

/** A SwiRL's argument within a sentence, which contains the name/type of the argument
  * and the starting & ending token offset of the argument in the sentence.
  *
  * @param name The name of the argument.
  * @param startOffset The starting token offset (inclusive) of the argument.
  * @param endOffset The ending token offset (exclusive) of the argument.
  */
case class SwiRLArgument(val name: String, var startOffset: Int, var endOffset: Int) {
  override def toString: String = {
    val os = new StringBuilder()
    os.append(name + ": [" + startOffset + ", " + endOffset + ")")
    os.toString
  }
}

/** A SwiRL parsed sentence, which contains a list of lemmatized tokens and a map of
  * semantic role labels for the sentence
  *
  * @param lemmas List of lemmas.
  * @param roleLabels Map of semantic role labels. Each role label is keyed by the token
  *                   index of the head predicate (e.g., index of "chase"), and the value for
  *                   each predicate is a list of associated arguments (e.g., arguments for
  *                   the "chase" predicate).
  */
case class SwiRLSentence(val lemmas: List[String], val roleLabels: Map[Int, ListBuffer[SwiRLArgument]]) {
  override def toString: String = {
    val os = new StringBuilder()
    os.append(lemmas.mkString(" ") + "\n")
    roleLabels.iterator.foreach(kv => {
      kv._2.foreach(arg => {
        os.append(" - " + lemmas(kv._1) + ": " + arg.name + " "
          + lemmas.slice(arg.startOffset, arg.endOffset).mkString(" ") + "\n")
      })
    })
    os.toString
  }
}

/** A parse error. */
case class SwiRLParseException(message: String) extends Exception(message)


/** SwiRL's document parser. Provides methods for reading a SwiRL's parsed document.
  */
object SwiRLParser {

  /** Load a SwiRL's parsed document from file.
    *
    * @param file File containing the SwiRL's parse.
    * @return A SwiRL's parsed document.
    */
  def load(file: File): SwiRLDocument = {
    val sentences = new ListBuffer[SwiRLSentence]()
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines()

    // First line specifies how many sentences
    val sentencesCount = lines.next().split(" ")(0).toInt

    // Parse sentences one at a time
    for (i <- 0 until sentencesCount) {
      // Sentences are delimited by an empty line
      if (lines.next().trim().length != 0)
        throw new SwiRLParseException("Expected an empty line before a sentence!")
      sentences.append(readSentence(lines))
    }
    source.close()

    // Create a document from the list of sentences
    new SwiRLDocument(sentences.toList)
  }

  /** Read the next parsed sentence from the lines of text.
    * @param linesIterator Lines of serialized text.
    * @return A SwiRL's parsed sentence.
    */
  private def readSentence(linesIterator: Iterator[String]): SwiRLSentence = {
    // Read in some meta data about the sentence
    var line = linesIterator.next().trim()
    val tokensCount = line.split(" ")(0).toInt
    linesIterator.next()
    if (linesIterator.next().trim().length != 0)
      throw new SwiRLParseException("Expected an empty line before between sentence formats!")

    // Skip the parse tree format, delimited by blank line
    do {
      line = linesIterator.next()
    } while (line.trim().length != 0)

    // Parse the table format
    val lemmas = new ListBuffer[String]()
    var parts: Array[String] = null
    var argText = ""
    val relationsHead = new ListBuffer[Int]()
    val relationsArgs = new ListBuffer[ListBuffer[SwiRLArgument]]()
    for (i <- 0 until tokensCount) {
      // Split each token line
      parts = linesIterator.next().split(" +")

      // The 3rd column is the token string (surrounded by quotes)
      lemmas.append(parts(2).replaceAll("^\"", "").replaceAll("\"$", ""))

      // The 4th column specifies whether or not this token is the head of a new relation
      if (parts(3).toInt == 1) relationsHead.append(i)

      // Initialize the list of relation arguments for each relation (do this only once, on first loop iteration)
      if (relationsArgs.size == 0) {
        // The number of columns after the 4th column is the number of relations
        for (i <- 0 until (parts.size - 4))
          relationsArgs.append(new ListBuffer[SwiRLArgument]())
      }

      // Parse to see if this token is the start or end of an argument for any of the relation
      for (j <- 0 until relationsArgs.length) {
        argText = parts(j + 4).trim()
        // Could be the start of a new argument (we'll leave the end offset as -1, to be filled in later).
        if (argText.startsWith("(")) {
          relationsArgs(j).append(new SwiRLArgument(argText.substring(1, argText.lastIndexOf("*")), i, -1))
        }
        // Or could be the end of the previous argument
        if (argText.endsWith(")")) {
          relationsArgs(j).last.endOffset = i + 1
        }
      }
    }

    // Construct mentions (aka. role labels) map from arguments
    if (relationsHead.size != relationsArgs.size)
      throw new SwiRLParseException("Unbalanced number of relation heads and arguments!")
    val mentions = new mutable.HashMap[Int, ListBuffer[SwiRLArgument]]()
    for (i <- 0 until relationsHead.size) {
      relationsArgs(i).foreach(arg => {
        // Sanity check, all arguments must have a valid start and end.
        if (!(0 <= arg.startOffset &&  arg.startOffset < arg.endOffset && arg.endOffset <= tokensCount))
          throw new SwiRLParseException("Invalid argument offsets! " + arg.toString)
        if (!mentions.contains(relationsHead(i)))
          mentions.put(relationsHead(i), new ListBuffer[SwiRLArgument]())
        mentions.get(relationsHead(i)).get.append(arg)
      })
    }

    return new SwiRLSentence(lemmas.toList, mentions.toMap)
  }

}


/** Demo: Load a SwiRL document from file */
object RunSwiRLDocument {

  def main(args: Array[String]) {
    val swirlFile = new File("/path/to/SwiRL-doc.txt")
    val swirlDoc = SwiRLParser.load(swirlFile)
    println(swirlDoc.toString)
  }

}
