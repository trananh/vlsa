package edu.arizona.sista.vlsa.corpus.parser

import edu.arizona.sista.vlsa.corpus
import edu.arizona.sista.vlsa.utils.IOUtils
import java.io._
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

/** A text parser for the Linguistic Data Consortium's English Gigaword
  * Corpus.  This parser iterates through the articles/documents
  * from the corpus one at a time.
  *
  * ''NOTE:'' This parser is adapted from the GigawordTextParser.java file
  * in the LingPipe toolkit, originally authored by Bob Carpenter.
  *
  * @constructor Create a new text parser for the Gigaword corpus.
  * @param corpusDir Root directory of the corpus.
  *
  * @author trananh
  */
class GigawordTextParser(val corpusDir: File) extends CorpusParser {

  /** Overloaded constructor */
  def this(corpusPath: String) = this(new File(corpusPath))

  /** Regex patterns to extract a document's type and ID */
  private val IDPattern: Regex = """id ?="([^"]*)"""".r
  private val TypePattern: Regex = """type ?="([^"]*)"""".r

  /** Initialize files iterator to go through all "*.gz" files from the root dir */
  private val filesIterator: Iterator[File] =
    IOUtils.getAllFilesRecursive(corpusDir, """.*\.gz$""".r).iterator

  /** Initialize file reader variables */
  private var file: Option[File] = None
  private var gzipStream: Option[InputStream] = None
  private var linesIterator: Option[Iterator[String]] = None

  /** The next article document in the corpus */
  private var nextDocument: Option[corpus.Document] = fetchNextDocument()

  /** Name of the corpus. */
  override def corpusName = "English Gigaword Corpus"
  
  /** Iterate through all documents in the corpus. */
  def iterator = new Iterator[corpus.Document] {

    /** Return true if there is another document in the corpus. */
    @throws(classOf[IOException])
    def hasNext: Boolean = {
      nextDocument.isDefined
    }

    /** Get the next document in the corpus. */
    @throws(classOf[IOException])
    def next(): corpus.Document = {
      // For efficiency, we always seek 1 document ahead
      val result = nextDocument.get
      nextDocument = fetchNextDocument()
      result
    }
  }

  /** Close all input streams and clear all memory references */
  def close() {
    if (gzipStream.isDefined)
      gzipStream.get.close()
    file = None
    gzipStream = None
    linesIterator = None
    nextDocument = None
  }

  /** Continue reading the corpus to retrieve the next document.
    * @return The next document in the corpus.
    */
  @tailrec
  @throws(classOf[IOException])
  private def fetchNextDocument(): Option[corpus.Document] = {

    // If we don't have an active/open file, then try to get the next one
    if (!file.isDefined) {

      // Return None if we ran out of files
      if (!filesIterator.hasNext)
        return None

      // Else, grab next file and create a new file iterator for reading
      file = Option(filesIterator.next())
      gzipStream = Option(new GZIPInputStream(new FileInputStream(file.get)))
      linesIterator = Option(Source.fromInputStream(gzipStream.get).getLines())
    }

    // Start/continue reading the file
    var line = ""
    while (linesIterator.get.hasNext) {
      line = linesIterator.get.next()

      // Skip lines until we get to an opening <DOC> tag
      var found: Option[String] = None
      var t = ""
      var id = ""
      if (line.startsWith("<DOC ")) {

        // Retrieve the article's type if exists
        t = ""
        found = TypePattern.findFirstIn(line)
        if (found.isDefined)
          t = found.get.substring(found.get.indexOf('"') + 1, found.get.lastIndexOf('"'))

        // Retrieve the article's unique ID if exists
        id = ""
        found = IDPattern.findFirstIn(line)
        if (found.isDefined)
          id = found.get.substring(found.get.indexOf('"') + 1, found.get.lastIndexOf('"'))

        // Skip to the <TEXT> tag for the article
        while (!line.startsWith("<TEXT>")) {
          assert(linesIterator.get.hasNext, {
            println("Document is missing the <TEXT> tag: " + id)
          })
          line = linesIterator.get.next()
        }

        // Start reading the text
        assert(line.startsWith("<TEXT>"), {
          println("This line should contain the <TEXT> tag: " + line)
        })
        val text = readTextBlock(linesIterator.get)

        // Assemble the document
        return Option(new corpus.Document(text,
          this.corpusName,
          mutable.Map[String, Any]("type" -> t, "id" -> id)))
      }
    }

    // Finished reading the current file, clean up readers
    gzipStream.get.close()
    file = None
    gzipStream = None
    linesIterator = None

    // We found no document, so recursively look in the next file
    fetchNextDocument()
  }

  /** Continue reading the corpus until we reach the closing </TEXT> tag.
    * @return The text block.
    */
  @throws(classOf[IOException])
  @throws(classOf[Exception])
  private def readTextBlock(in: Iterator[String]): String = {
    var line = ""
    val sb = new StringBuilder()
    var continuing = false

    // Read in text
    while (in.hasNext) {
      line = in.next()
      if (line.startsWith("</TEXT>")) {
        // Reached the end of the text content, return the text
        return sb.toString().trim()
      }
      if (line.startsWith("<P>")) {
        continuing = false
        sb.append("\t")
      } else if (!line.startsWith("</P>")) {
        if (continuing)
          sb.append(" ")
        else
          continuing = true
        // Append line (remove escapes if necessary)
        sb.append(if (line.indexOf('&') >= 0) removeEscapes(line) else line)
      }
    }

    // Shouldn't be here!  It would mean we have reached the end of
    // 	the file without finishing the text block.
    throw new Exception("Missing ending </TEXT> tag in File: " + file.get.getName)
  }

  /** Remove escape characters from a string.
    * @return New string with no escape characters.
    */
  private def removeEscapes(line: String): String = {
    line.replaceAll("&(amp|AMP);", "&").replaceAll("&(lt|LG);", "<").replaceAll("&(gt|GT);", ">")
  }
  
}


object RunGigawordTextParser {

  /** A demo application that parses the Gigaword corpus and extracts the contents of all
    * "story" articles to a file.
    */
  def main(args: Array[String]) {
    // NOTE: change hardcoded paths below
    val corpusDir = "/Users/trananh/Workspace/Data/text/corpora/Gigaword/data"
    val outFile = "/Users/trananh/Workspace/Data/text/corpora/Gigaword-story/giga-story"

    val parser = new GigawordTextParser(corpusDir)

    // Create new file
    val file = new File(outFile)
    if (file.exists()) file.delete()
    file.createNewFile()

    // Iterate through and concatenate each doc to file
    var count = 0
    var next = parser.parseNext()
    val sb = new StringBuilder()
    while (next.isDefined) {
      if (next.get.attributes.getOrElse("type", "other").equals("story")) {
        sb.append(next.get.text + "\n")

        if (sb.size > 10485760) {
          // Flush every 10 megabyte
          flushFile(file, sb.toString())
          sb.clear()
        }
      }

      count += 1
      if (count % 10000 == 0)
        println("Docs completed: " + count)

      // Get next one
      next = parser.parseNext()
    }

    println("Docs completed: " + count)
  }

  /** Flush the contents to file.
    * @param file File to be appended.
    * @param contents The contents.
    */
  def flushFile(file: File, contents: String) {
    // Write to outfile, if it's defined.
    try {
      val writer = new FileWriter(file, true)
      writer.write(contents)
      writer.close()
    } catch {
      case ioe: IOException => println("IOException: " + ioe.getMessage())
      case e: Exception => println(e.printStackTrace())
    }
  }

}