package edu.arizona.sista.vlsa.search.lucene

import java.text.BreakIterator
import java.util.Locale
import org.apache.lucene.search.postingshighlight.{Passage, PassageFormatter}

/** Creates a formatted snippet from the top passages.
  *
  * Implementation is based on Lucene postings highlight's PassageFormatter.
  *
  * The default implementation marks the query terms as bold, and places
  * ellipses between disconnected passages, while also including up to
  * 1 preceding and 1 following passage for each hit.
  *
  * <b>WARNING</b>: This class is only guaranteed to be compatible with
  * the PostingsHighlighter from LUCENE version 4.2.1.
  *
  * @constructor Create a snippet formatter with windowing capability.
  * @param window The window of passages before & after the hit passage (default 1).
  * @param preTag Tag to insert before each search term in the highlight.
  * @param postTag Tag to insert after each search in the highlight.
  * @param ellipsis Tag to insert between disconnected passages in the highlight.
  *                 As of (10/08/13), ellipses are inserted even if passages are not
  *                 disconnected but line up end-to-end, as long as they don't overlap.
  * @param bi Method of breaking the content into passages (default to sentences).
  *
  * @author trananh
  */
class WindowPassageFormatter(
    val window: Integer = 1,
    preTag: String = "<b>",
    postTag: String = "</b>",
    ellipsis: String = "...",
    val bi: BreakIterator = BreakIterator.getSentenceInstance(Locale.ROOT))
    extends PassageFormatter(preTag, postTag, ellipsis) {

  /** Formats the top <code>passages</code> from <code>content</code>
    * into a human-readable text snippet.  Also include passages that
    * preceded and follow each hit passage (i.e., a window of passages).
    *
    * Implementation is based on Lucene postings highlight's PassageFormatter.
    *
    * @param passages Top-N passages for the field. Note these are sorted in
    *                 the order that they appear in the document for convenience.
    * @param content Content for the field.
    *
    * @return Formatted highlight.
    */
  override def format(passages: Array[Passage], content: String): String = {

    // Use break iterator to segment passages
    bi.setText(content)

    val sb = new StringBuilder()
    var current = 0
    var pos = 0
    for (i <- 0 until passages.length) {
      val passage = passages(i)

      // Pre-pend stuff before the actual passage (e.g., window & ellipsis)
      // A.T. (10/08/13): add ellipses even if passages line up end-to-end, as long as they don't overlap.
      if (passage.getStartOffset > 0 && (current == 0 || passage.getStartOffset >= current)) {
        // This is either the first hit passage or a subsequent hit passage that starts after the previous one
        if (window > 0) {
          // In both cases, prepend to it. Though, we need to take care not to prepend pass an existing passage,
          // and also add ellipsis if necessary.
          var ws = bi.preceding(passage.getStartOffset - 1)
          var j = window - 1
          while (j > 0 && ws > current) {
            // seek past passages
            ws = bi.previous()
            j -= 1
          }
          if (current > 0 && ws >= current) {
            // If the window's start boundary is after or line-up with the current offset then add ellipsis.
            sb.append(ellipsis)
          }
          // Now prepend full or partial window
          sb.append(content.substring(Math.max(current, ws), passage.getStartOffset))
        } else if (current > 0) {
          // If not windowing, then just add ellipsis when applicable
          sb.append(ellipsis)
        }
        current = passage.getStartOffset
      }

      // Now add the actual passage, with the appropriate pre-tag and post-tag around matches.
      pos = current
      for (i <- 0 until passage.getNumMatches) {
        val start = passage.getMatchStarts()(i)
        val end = passage.getMatchEnds()(i)
        // its possible to have overlapping terms
        if (start > pos) {
          sb.append(content.substring(pos, start))
        }
        if (end > pos) {
          sb.append(preTag)
          sb.append(content.substring(Math.max(pos, start), end))
          sb.append(postTag)
          pos = end
        }
      }
      // its possible a "term" from the analyzer could span a sentence boundary.
      sb.append(content.substring(pos, Math.max(pos, passage.getEndOffset)))

      // Update current
      current = Math.max(pos, passage.getEndOffset)

      // Append stuff after the actual passage
      if (i + 1 >= passages.length || current < passages(i + 1).getStartOffset) {
        // This is either the last hit passage, or some arbitrary passage where
        // the next passage is not connected to it.
        if (window > 0 && current < content.length()) {
          // In these cases, append to it.  We need to take care not to append
          // pass the start of the next passage.
          val limit = if (i + 1 < passages.length) passages(i + 1).getStartOffset else content.length()
          var next = bi.following(current - 1)
          var we = if (next != BreakIterator.DONE) next else current
          var j = window
          while (j > 0 && next != BreakIterator.DONE && we < limit) {
            next = bi.next()
            we = if (next != BreakIterator.DONE) next else we
            j -= 1
          }
          // Append the window, but don't go pass the limit
          sb.append(content.substring(current, Math.min(we, limit)))
          current = Math.min(we, limit)
        }
      }
    }

    sb.toString()
  }

}