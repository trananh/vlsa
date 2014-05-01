package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import edu.arizona.sista.processors._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.struct.DirectedGraphEdgeIterator
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.models.data.{SwiRLParser, SwiRLSentence, SwiRLDocument}
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import edu.arizona.sista.vlsa.utils.{Constants, NLPUtils}
import java.io._
import scala.collection.mutable
import scala.collection.mutable.{Map, HashMap, ListBuffer}
import util.control.Breaks._

/** A word-search model to find the occurrence frequency of keywords within some neighborhood
  * of some search activity.  This model operates specifically on results retrieved from an index
  * corpus.  It improves over the simple DocNeighborhood model by using coref to extend the
  * highlight snippets of search results
  *
  * @constructor A coref-extended word-neighborhood model that operates on an indexed corpus.
  *
  * @author trananh
  */
class CorefNeighborhood {

  /** Maintain a map of each keyword to its base form. */
  val keywordsMap = new mutable.HashMap[String, String]()

  /** Dictionary of words that we're interested in */
  val dictionary = new FrequencyDictionary[String]()

  /** CoreNLP processor and serializer */
  val processor: Processor = new CoreNLPProcessor(internStrings = false)
  val serializer = new DocumentSerializer()

  /** Track some helpful statistics */
  var totalActivityInstances = 0
  var totalSuccessfulNLPArgsParses = 0
  var totalFailureNLPArgsParses = 0
  var totalSuccessfulSwiRLArgsParses = 0
  var totalFailureNLPAndSwiRLArgsParses = 0
  var totalSwiRLParseErrors = 0
  var totalExtendedSentences = 0

  /** Clear all open streams and internal memory usage. */
  def clear() {
    dictionary.clear()
    keywordsMap.clear()
  }

  /** Reset frequency count for all tokens. */
  def reset() {
    dictionary.reset()
  }

  /** Load list of keywords that we're interested in.
    * All keywords are passed through lemmatization to recover the base form.
    *
    * Example file contents:
    * word1
    * .
    * .
    * wordN
    *
    * @param dictFile File containing the list of words.
    */
  def loadDictionary(dictFile: File) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTerms(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      dictionary.createEntry(e._2)
      keywordsMap.put(e._1, e._2)
    })
    source.close()
  }

  /** Load list of keyword adjectives that we're interested in.
    * All keywords are passed through lemmatization to recover the base form.
    *
    * Example file contents:
    * adjective1
    * .
    * .
    * adjectiveN
    *
    * @param dictFile File containing the list of adjectives.
    */
  def loadAdjectiveDictionary(dictFile: File) {
    // Lemmatize each word and add the base form to the dictionary
    val source = scala.io.Source.fromFile(dictFile)
    val terms = source.getLines().map(_.trim.toLowerCase).toArray
    val lemmas = NLPUtils.lemmatizeTermsAsAdjectives(terms).map(_.trim.toLowerCase)
    terms.zip(lemmas).foreach(e => {
      dictionary.createEntry(e._2)
      keywordsMap.put(e._1, e._2)
    })
    source.close()
  }

  /** Load the CoreNLP annotation from file.
    * @param nlpFile File containing the annotation (assumed to exist).
    * @return The annotation.
    */
  protected def loadNLP(nlpFile: File): Document = {
    assert(nlpFile.exists())
    val is = new FileInputStream(nlpFile)
    val nlpDoc = serializer.load(is)
    is.close()
    nlpDoc
  }

  /** Load the SwiRL annotation from file if the file exists and the annotation
    * is well-formed (formatted correctly).
    * @param swirlFile File containing the annotation.
    * @return The annotation if it can be parsed correctly, else None.
    */
  protected def loadSwiRL(swirlFile: Option[File]): Option[SwiRLDocument] = {
    var swirlDoc: Option[SwiRLDocument] = None
    if (swirlFile.isDefined && swirlFile.get.exists()) {
      try {
        swirlDoc = Option(SwiRLParser.load(swirlFile.get))
      } catch {
        case e: Exception =>
          // Unable to parse the swirl file
          swirlDoc = None
          totalSwiRLParseErrors += 1
      }
    }
    swirlDoc
  }

  /** Find mentions of the subject & object for each instance of the activity in the text
    * and then return the coref chains associated with each of the actor. That is, for each object
    * and subject of the activity, return the coref-chains describing their mentions in the
    * document. This method only uses CoreNLP annotation.
    *
    * @param activity The activity to find arguments for.
    * @param text The text to find coref-chains for.
    *
    * @return A tuple of lists of coref-chains (subject-chains, object-chains).
    */
  def findArgsCorefChains(activity: String, text: String): (List[Array[CorefMention]], List[Array[CorefMention]]) = {
    val nlpDoc = processor.annotate(text)
    findArgsCorefChains(activity, nlpDoc)
  }

  /** Find mentions of the subject & object for each instance of the activity in the document
    * and then return the coref chains associated with each of the actor. That is, for each object
    * and subject of the activity, return the coref-chains describing their mentions in the
    * document.
    *
    * @param activity The activity to find arguments for.
    * @param nlpFile File containing the Core-NLP annotation for the document.
    *
    * @return A tuple of lists of coref-chains (subject-chains, object-chains).
    */
  def findArgsCorefChains(activity: String, nlpFile: File): (List[Array[CorefMention]], List[Array[CorefMention]]) = {
    findArgsCorefChains(activity, nlpFile, None)
  }

  /** Find mentions of the subject & object for each instance of the activity in the document
    * and then return the coref chains associated with each of the actor. That is, for each object
    * and subject of the activity, return the coref-chains describing their mentions in the
    * document.
    *
    * @param activity The activity to find arguments for.
    * @param nlpFile File containing the Core-NLP annotation for the document.
    * @param swirlFile File containing the SwiRL annotation for the document (if exists). Defaults to None.
    *
    * @return A tuple of lists of coref-chains (subject-chains, object-chains).
    */
  def findArgsCorefChains(activity: String, nlpFile: File, swirlFile: Option[File])
  : (List[Array[CorefMention]], List[Array[CorefMention]]) = {
    // Load core-nlp annotation from file
    val nlpDoc = loadNLP(nlpFile)

    // Load swirl annotation if exists and can be parsed
    val swirlDoc = loadSwiRL(swirlFile)

    // Find the coref-chains associated with each argument
    findArgsCorefChains(activity, nlpDoc, swirlDoc)
  }

  /** Find mentions of the subject & object for each instance of the activity in the document
    * and then return the coref chains associated with each of the actor. That is, for each object
    * and subject of the activity, return the coref-chains describing their mentions in the
    * document.
    *
    * @param activity The activity to find arguments for.
    * @param nlpDoc The Core-NLP annotation for the document.
    * @param swirlDoc The SwiRL annotation for the document (if exists). Defaults to None.
    *
    * @return A tuple of lists of coref-chains (subject-chains, object-chains).
    */
  def findArgsCorefChains(activity: String, nlpDoc: Document, swirlDoc: Option[SwiRLDocument] = None)
  : (List[Array[CorefMention]], List[Array[CorefMention]]) = {

    if (Constants.DEBUG)
      println("\n\n\n\n==== DOCUMENT ====\n" + nlpDoc.sentences.map(s => s.words.mkString(" ")).mkString(" "))

    // Check to see if we also have SwiRL's annotation
    val isValidSwirlDoc = swirlDoc.isDefined && nlpDoc.sentences.length == swirlDoc.get.sentences.size

    /**********************************************************************************
      * Step 1: First, we need to do some dependency parsing to resolve the arguments.
      * That means finding out which phrase (or mention) refer to the subject and
      * which refer to the object of the activity.
      */
    if (Constants.DEBUG) println("\n\n==> Step 1: PARTICIPANTS IDENTIFICATION")

    // Maps to store discovered subject & object mentions
    //  -> nlpMaps: key = sentence index, value = set of mention head indices (recognized by coref engine).
    //  -> swirlMaps: key = sentence index, value = set of mentions (start & end offsets).
    val subNLPMentions: Map[Int, Set[Int]] = new HashMap[Int, Set[Int]]()
    val objNLPMentions: Map[Int, Set[Int]] = new HashMap[Int, Set[Int]]()
    val subSwirlMentions: Map[Int, Set[(Int, Int)]] = new HashMap[Int, Set[(Int, Int)]]()
    val objSwirlMentions: Map[Int, Set[(Int, Int)]] = new HashMap[Int, Set[(Int, Int)]]()

    // Now the actual work, iterate over each sentence and find mentions of associated actors
    for (i <- 0 until nlpDoc.sentences.size) {

      // Identify all relevant mentions from the Core-NLP annotation (this uses CoreNLP syntactic dependencies)
      val nlpMentions = identifyArgsNLP(activity, nlpDoc.sentences(i))

      // Variables to hold the argument mentions for this sentence
      val nlpSubjects = new ListBuffer[Int]()
      val nlpObjects = new ListBuffer[Int]()
      val swirlSubjects = new ListBuffer[(Int, Int)]()
      val swirlObjects = new ListBuffer[(Int, Int)]()

      // For each activity instance found, if we cannot find both the subject & object with NLP
      // then try SwiRL.  However, if SwiRL also fails to find both subject & object, then we
      // default to using NLP's results.
      nlpMentions.foreach(kv => {
        totalActivityInstances += 1

        if ((kv._2._1.size > 0 && kv._2._2.size > 0) || (!isValidSwirlDoc)) {
          // If both object and subject are present (i.e., lists aren't empty), then add them and we're done
          nlpSubjects.appendAll(kv._2._1)
          nlpObjects.appendAll(kv._2._2)

          // Count how many times NLP succeeded in finding both arguments
          if (kv._2._1.size > 0 && kv._2._2.size > 0)
            totalSuccessfulNLPArgsParses += 1
          else if (kv._2._1.size == 0 && kv._2._2.size == 0)
            totalFailureNLPArgsParses += 1

        } else if (isValidSwirlDoc
          && nlpDoc.sentences(i).lemmas.get.length == swirlDoc.get.sentences(i).lemmas.length) {

          // Else if NLP failed to find both, we'll let SwiRL try
          val swirlMentions = identifyArgsSwiRL(Set(kv._1), swirlDoc.get.sentences(i))
          
          // See if SwiRL succeeded in finding both arguments.
          if (swirlMentions.contains(kv._1) && swirlMentions.get(kv._1).get._1.size > 0
            && swirlMentions.get(kv._1).get._2.size > 0) {

            // If SwiRL is able to find both the subject and object, then use SwiRL
            swirlSubjects.appendAll(swirlMentions.get(kv._1).get._1)
            swirlObjects.appendAll(swirlMentions.get(kv._1).get._2)

            // Count how many times SwiRL succeeded in finding both
            totalSuccessfulSwiRLArgsParses += 1
          } else {

            // Else if SwiRL also fails to find both arguments, then we default back to using NLP's output
            // if NLP got at least one.  If NLP failed both then we must rely on SwiRL, and if both NLP
            // and SwiRL failed, then we're out of luck.
            if (kv._2._1.size > 0 || kv._2._2.size > 0) {
              nlpSubjects.appendAll(kv._2._1)
              nlpObjects.appendAll(kv._2._2)
            } else if (swirlMentions.contains(kv._1)) {
              swirlSubjects.appendAll(swirlMentions.get(kv._1).get._1)
              swirlObjects.appendAll(swirlMentions.get(kv._1).get._2)
              totalFailureNLPArgsParses += 1
            } else {
              totalFailureNLPAndSwiRLArgsParses += 1
            }

          }
        }
      })

      // Record the mentions for all activity instances, mapped by the sentence index
      if (nlpSubjects.size > 0) subNLPMentions.put(i, nlpSubjects.toSet)
      if (nlpObjects.size > 0) objNLPMentions.put(i, nlpObjects.toSet)
      if (swirlSubjects.size > 0) subSwirlMentions.put(i, swirlSubjects.toSet)
      if (swirlObjects.size > 0) objSwirlMentions.put(i, swirlObjects.toSet)
    }


    /**********************************************************************************
      * Step 2: Now that we know what phrases refer to the valid arguments of interest,
      * find coref-chains involving these phrases.
      */
    if (Constants.DEBUG) println("\n\n==> Step 2: COREFERENCE RESOLUTION")

    // Find chains for the subjects
    if (Constants.DEBUG) println("\nCoref chain(s) for SUBJECTS:")
    val subChains = identifyArgsCorefChains(subNLPMentions, subSwirlMentions, nlpDoc)

    // Find chains for the objects
    if (Constants.DEBUG) println("\nCoref chain(s) for OBJECTS:")
    val objChains = identifyArgsCorefChains(objNLPMentions, objSwirlMentions, nlpDoc)

    // Return the chains
    return (subChains, objChains)

  }

  /** Identify the arguments associated with the specified activity in the sentence.
    * Specifically, identify the subject (aka agent) and object (aka patience) arguments.
    * We do this by iterating over the list of syntactic dependencies and look for
    * specific patterns of dependencies that involve the desired activity.
    *
    * @param activity The activity to find arguments for.
    * @param sentence The Core-NLP annotated sentence.
    *
    * @return A map whose keys = the token indices of the activity (since the sentence could
    *         contain multiple instances of the activity), and the value for each key = a
    *         tuple containing two lists of head indices (subject, object) associated with
    *         that unique activity instance.
    */
  protected def identifyArgsNLP(activity: String, sentence: Sentence): Map[Int, (ListBuffer[Int], ListBuffer[Int])] = {
    // Map of each activity head to its lists of subject and object head indices.
    val activityMap: Map[Int, (ListBuffer[Int], ListBuffer[Int])] =
      new HashMap[Int, (ListBuffer[Int], ListBuffer[Int])]()

    // Skip empty sentences
    if (!sentence.lemmas.isDefined) {
      return activityMap
    }

    // Find all occurrences of the activity, but only consider verbs.
    val activityLemmas = sentence.lemmas.get.zipWithIndex.filter(_._1.toLowerCase.equalsIgnoreCase(activity))
    val activityHeadIdx = activityLemmas.map(_._2).filter(i => NLPUtils.POS_VBs.contains(sentence.tags.get(i))).toSet

    // If the sentence does contain a verb occurrence of the activity, then process each
    // verb instance to find the subject & object arguments.
    if (activityHeadIdx.size > 0) {

      // For each occurrence of the activity, find its subject (1st index) and object (2nd index)
      activityHeadIdx.foreach(idx => activityMap.put(idx, (new ListBuffer[Int](), new ListBuffer[Int]())))

      if (Constants.DEBUG) println("\n\"" + sentence.words.mkString(" ") + "\"")
      val activityDependencies = new ListBuffer[(Int, Int, String)]()

      // We find the arguments by first iterating over the list of syntactic dependencies and finding
      // dependencies associated with the activity.
      val iterator = new DirectedGraphEdgeIterator[String](sentence.dependencies.get)
      while (iterator.hasNext) {
        val dep = iterator.next

        // Find dependencies where the activity verb is the head.
        if (activityHeadIdx.contains(dep._1)) {
          activityDependencies.append(dep)
        }
      }

      // Now that we have a list of dependencies associated with the activity, look for other
      // dependencies that are associated with these "activity" dependencies.
      if (activityDependencies.size > 0) {

        // Retrieve the types of all other dependencies related to our key activity dependencies.
        val dependencyTypes = activityDependencies.map(d => (d._3, d)).toMap
        var d: (Int, Int, String) = null

        // From the list of associated dependencies, seek for the specific patterns that we want!
        if (dependencyTypes.contains("agent")) {

          /* Pattern: "A man was chased _by_ the police."
           * => man is the nsubjpass and police is the agent.
           *
           * "I was hugged _by_ her."
           * => I is the nsubjpass and her is the agent.
           */

          d = dependencyTypes.get("agent").get
          activityMap.get(d._1).get._1.append(d._2)
          if (Constants.DEBUG)
            println("Head: \"" + sentence.words(d._1) + ":" + d._1 + "\";\t NLPSub: \"" + sentence.words(d._2) + "\"")

          if (dependencyTypes.contains("nsubjpass")) {
            d = dependencyTypes.get("nsubjpass").get
            activityMap.get(d._1).get._2.append(d._2)
            if (Constants.DEBUG)
              println("Head: \"" + sentence.words(d._1) + ":" + d._1 + "\";\t NLPObj: \"" + sentence.words(d._2) + "\"")
          }

        } else {

          /* Patterns involving dobj and sometimes nsubj.
           *
           * "A man chases a dog."
           * "A man chased a dog."
           * "A man was chasing a dog."
           * "A man used the car to chase the dog."
           * "A man frightened the dog to chase it."
           *
           * "A man hugs his wife."
           * "A man hugged his wife."
           * "A man was hugging his wife."
           */

          if (dependencyTypes.contains("nsubj")) {
            d = dependencyTypes.get("nsubj").get
            activityMap.get(d._1).get._1.append(d._2)
            if (Constants.DEBUG)
              println("Head: \"" + sentence.words(d._1) + ":" + d._1 + "\";\t NLPSub: \"" + sentence.words(d._2) + "\"")
          }

          if (dependencyTypes.contains("dobj")) {
            d = dependencyTypes.get("dobj").get
            activityMap.get(d._1).get._2.append(d._2)
            if (Constants.DEBUG)
              println("Head: \"" + sentence.words(d._1) + ":" + d._1 + "\";\t NLPObj: \"" + sentence.words(d._2) + "\"")
          }

          if (activity.equalsIgnoreCase("chase") && dependencyTypes.contains("prep_after")) {
            /* Pattern: "A man chased after a dog."
             * This pattern is perhaps only specific to "chase" (non-general). */
            d = dependencyTypes.get("prep_after").get
            activityMap.get(d._1).get._2.append(d._2)
            if (Constants.DEBUG)
              println("Head: \"" + sentence.words(d._1) + ":" + d._1 + "\";\t NLPObj: \"" + sentence.words(d._2) + "\"")
          }

        }

      } // end processing each activity verb dependency

    }

    // Return the dependencies map (aka the arguments map)
    activityMap
  }

  /** Identify the arguments associated with the specified activity instances in the sentence.
    * Specifically, identify the subject (aka agent) and object (aka patience) arguments.
    *
    * @param activityHeadIdx The head indices of the activity that we are interested in.
    * @param sentence The SwiRL annotated sentence.
    *
    * @return A map whose keys = the token indices of the activity (since the sentence could
    *         contain multiple instances of the activity), and the value for each key = a
    *         tuple containing two lists of mention offsets (subject, object) associated with
    *         that unique activity instance.
    */
  protected def identifyArgsSwiRL(activityHeadIdx: Set[Int], sentence: SwiRLSentence)
  : Map[Int, (ListBuffer[(Int, Int)], ListBuffer[(Int, Int)])] = {
    // Map of each activity head to its lists of subject and object mentions (start & end offset)
    val activityMap: Map[Int, (ListBuffer[(Int, Int)], ListBuffer[(Int, Int)])] =
      new HashMap[Int, (ListBuffer[(Int, Int)], ListBuffer[(Int, Int)])]()

    // For each activity instance (represented by its token index), we find all associated role labels.
    activityHeadIdx.foreach(headIdx => {

      if (sentence.roleLabels.contains(headIdx)) {
        activityMap.put(headIdx, (new ListBuffer[(Int, Int)](), new ListBuffer[(Int, Int)]()))

        // For each activity head index, find the swirl arguments that are associated with that index.
        sentence.roleLabels.get(headIdx).get.foreach(arg => {

          // We are particularly interested in arguments of types: A0 (subject), and A1 (object)
          if (arg.name.equalsIgnoreCase("A0")) {
            // A0 would be the subject
            activityMap.get(headIdx).get._1.append((arg.startOffset, arg.endOffset))
            if (Constants.DEBUG)
              println("Head: \"" + sentence.lemmas(headIdx) + ":" + headIdx
                + "\";\t SwirlSub: \"" + sentence.lemmas.slice(arg.startOffset, arg.endOffset).mkString(" ") + "\"")
          } else if (arg.name.equalsIgnoreCase("A1")) {
            // A1 is the object
            activityMap.get(headIdx).get._2.append((arg.startOffset, arg.endOffset))
            if (Constants.DEBUG)
              println("Head: \"" + sentence.lemmas(headIdx) + ":" + headIdx
                + "\";\t SwirlObj: \"" + sentence.lemmas.slice(arg.startOffset, arg.endOffset).mkString(" ") + "\"")
          }
        })
      }
    })

    // Return the dependencies map (aka the arguments map)
    activityMap
  }

  /** Identify the coref-chains containing the specified arguments mentions.
    *
    * @param nlpMentionsMap Mentions mapping identified from core-NLP annotation, mapping the sentence
    *                       index to the set of mention head indices in that sentence.
    * @param swirlMentionsMap Mentions mapping identified from swirl annotation, mapping the sentence
    *                         index to the set of mentions (start & end offsets) in that sentence.
    * @param nlpDoc The Core-NLP annotation.
    *
    * @return A list of coref-chains associated with the given mentions.
    */
  protected def identifyArgsCorefChains(nlpMentionsMap: Map[Int, Set[Int]],
                                        swirlMentionsMap: Map[Int, Set[(Int, Int)]],
                                        nlpDoc: Document): List[Array[CorefMention]] = {
    // List of found coref-chains
    val mentionsChains = new ListBuffer[Array[CorefMention]]()

    // It is a bit tricky to find coref-chains for swirl mentions. An arbitrary swirl mention
    // does not always line up perfection with an NLP coref-mention, in fact it could be
    // mapped to several possible coref-mentions where each coref-mention could belong to a different
    // coref-chain.  So we'll need to map each swirl mention to a list of possible coref-chains, and
    // pick the one with the highest score (based on some overlapping criteria).
    val swirlMappings = new HashMap[(Int, Int), ListBuffer[(Double, Array[CorefMention])]]()

    // Iterate over all chains to find ones that include a mention from the maps.
    for (chain <- nlpDoc.coreferenceChains.get.getChains) {

      // Convert the iterable chain to an array.
      val chainArray = chain.toArray

      // Iterate through all mentions in the chain and check to see if anyone of them is equivalent
      // to one from our mentions (from our maps).
      breakable {
        for (i <- 0 until chainArray.size) {
          // The current coref-mention
          val m = chainArray(i)

          if (nlpMentionsMap.getOrElse(m.sentenceIndex, Set.empty[Int]).contains(m.headIndex)) {

            // Looks like we've found an identical mention from the NLP map, so add the chain and move on.
            mentionsChains.append(chainArray.clone())
            break()

          } else if (swirlMentionsMap.contains(m.sentenceIndex)) {

            // Else if this coref-mention belongs to the same sentence as one of the Swirl's mention, then
            // we'll need to do some checking to see if there's an equivalent swirl mention.
            var found = false
            var foundRatio = 0.3      // intersection / union ratio
            var foundKey = (0, 0)
            var foundInterval = (0, 0)

            swirlMentionsMap.get(m.sentenceIndex).get.foreach(sm => {
              // First of all, the swirl mention better contain the headword index of this coref mention
              if (sm._1 <= m.headIndex && m.headIndex < sm._2) {

                val int = math.min(sm._2, m.endOffset) - math.max(sm._1, m.startOffset)
                val union = math.max(sm._2, m.endOffset) - math.min(sm._1, m.startOffset).toFloat
                if (int > 0 && (int / union) >= foundRatio) {
                  // Second of all, the swirl mention should overlap with the coref mention by some
                  // minimum threshold, where overlap = intersection / union.
                  found = true
                  foundKey = sm
                  foundInterval = (math.min(sm._1, m.startOffset), math.max(sm._2, m.endOffset))
                  foundRatio = int / union
                }
              }
            })

            if (found) {
              // If we found a valid swirl mention, then update the mention in the coref-chain so that
              // the coref-mention now describes the union of the two mentions.
              val copy = chainArray.clone()
              copy(i) = new CorefMention(m.sentenceIndex, m.headIndex, foundInterval._1, foundInterval._2, m.chainId)

              // Add the chain as a possible candidate for this swirl mention (the score will be the overlap ratio)
              if (!swirlMappings.contains(foundKey))
                swirlMappings.put(foundKey, new ListBuffer[(Double, Array[CorefMention])])
              swirlMappings.get(foundKey).get.append((foundRatio, copy))
              break()
            }

          }
        } // end inner chain loop
      }

    } // end outer chains loop

    // For swirl, we may have to disambiguate the coref-chains
    swirlMappings.iterator.foreach(kv => {
      val bestChain = kv._2.sortWith(_._1 > _._1).head._2
      mentionsChains.append(bestChain)
    })

    // Print valid chains (for DEBUG)
    if (Constants.DEBUG) {
      mentionsChains.foreach(chain => {
        println("One chain found containing the following mentions:")
        for (mention <- chain) {
          println("\tsentence (" + mention.sentenceIndex + "): " +
            nlpDoc.sentences(mention.sentenceIndex).words.slice(0, mention.startOffset).mkString(" ") + " [" +
            nlpDoc.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString(" ")
            + "] " + nlpDoc.sentences(mention.sentenceIndex).words.slice(mention.endOffset,
            nlpDoc.sentences(mention.sentenceIndex).words.length).mkString(" "))
        }
      })
    }

    // Return the coref-chains
    mentionsChains.toList
  }

  /** Get highlights stream from cached file.
    *
    * @param cacheFile Path to file containing the cached highlights.
    *
    * @return A stream of string iterators to iterate through each cached highlight.
    */
  def getCachedHighlightsStream(cacheFile: File): Iterator[String] = {
    assert(cacheFile.exists())
    return scala.io.Source.fromFile(cacheFile).getLines()
  }

  /** A more memory-efficient method of retrieving coref-expanded highlights that does not load all highlights
    * into memory at once. Highlights are retrieved one by one and written to a cached file, and then
    * read back as a stream.
    *
    * @param activity The activity.
    * @param nlpDir The directory containing NLP annotations (one file per document).
    * @param swirlDir The directory containing Swirl annotations if exist. Defaults to None.
    * @param cacheFile Path to file containing the cached highlights.  If the file does not already
    *                  exist, new coref process will be performed and the results cached to the file.
    *                  We need to use a cache file for efficiency, to help lazily load the highlights
    *                  so that we don't need to keep all highlight passages in memory.
    *
    * @return A stream of string iterators to iterate through each coref-expanded highlight.
    */
  def getCorefHighlightsStream(activity: String, nlpDir: File, swirlDir: Option[File] = None, cacheFile: File)
  : Iterator[String] = {

    // Load cached data if already exists
    if (cacheFile.exists()) {
      return getCachedHighlightsStream(cacheFile)
    }
    if (Constants.DEBUG) println("Cache file not found! Performing new coref search.")

    // Init some variables
    var corefChains: (List[Array[CorefMention]], List[Array[CorefMention]]) = null
    val sentences = new ListBuffer[Int]()

    // Cache the new results so that we can re-load it with an iterator
    val out = new java.io.FileWriter(cacheFile)
    var first = true

    // Read each annotation file and find the coref-chains
    nlpDir.listFiles().foreach(nlpFile => {

      if (nlpFile.getName.endsWith(".txt")) {

        if (Constants.DEBUG) println("\n\nProcessing document: " + nlpFile.getName)
        sentences.clear()

        // Load CoreNLP annotation from file
        val nlpDoc = loadNLP(nlpFile)

        // Load SwiRL annotation if exists
        var swirlDoc: Option[SwiRLDocument] = None
        if (swirlDir.isDefined && swirlDir.get.exists()) {
          swirlDoc = loadSwiRL(Option(new File(swirlDir.get.getAbsolutePath + "/" + nlpFile.getName)))
        }

        try {
          // Find the coref chains and extract the sentence indices
          corefChains = findArgsCorefChains(activity, nlpDoc, swirlDoc)
          corefChains._1.foreach(chain => chain.foreach(mention => sentences.append(mention.sentenceIndex)))
          corefChains._2.foreach(chain => chain.foreach(mention => sentences.append(mention.sentenceIndex)))

          if (sentences.size > 0) {
            // Concatenate the the sentences
            val sb = new StringBuilder()
            collection.SortedSet(sentences: _*).foreach(i => {
              sb.append(nlpDoc.sentences(i).words.mkString(" ") + " " )

              this.totalExtendedSentences += 1
            })

            // Write to file
            if (first) {
              out.write(sb.toString().trim())
              first = false
            } else {
              out.write("\n" + sb.toString().trim())
            }
          }
        } catch {
          case e: Exception => println("ERROR file: " + nlpFile.getName + "\n")
          e.printStackTrace()
        }
      }

    }) // end loop
    out.close

    // Return the loaded file
    getCachedHighlightsStream(cacheFile)
  }

  /** A more memory-efficient method of retrieving window-expanded highlights that does not load all highlights
    * into memory at once. Highlights are retrieved one by one and written to a cached file, and then
    * read back as a stream.
    *
    * We find the highlights by finding sentences containing the proper use of the activity (i.e., in its
    * verb form).  Then we form an extended highlight by retrieving the before and following sentences
    * within the window boundary.  For example, window = 1 means retrieve 1 sentence before and 1 sentence
    * after, resulting in 3 sentences.
    *
    * @param activity The activity (expected to be a verb).
    * @param nlpDir The directory containing NLP annotations (one file per document).
    * @param window The window of sentences, defaults to 1.
    * @param cacheFile Path to file containing the cached highlights.  If the file does not already
    *                  exist, a new search will be performed and the results cached to the file.
    *                  We need to use a cache file for efficiency, to help lazily load the highlights
    *                  so that we don't need to keep all highlight passages in memory.
    *
    * @return A stream of string iterators to iterate through each window-expanded highlight.
    */
  def getWindowHighlightsStream(activity: String, nlpDir: File, window: Int = 1, cacheFile: File)
  : Iterator[String] = {

    // Load cached data if already exists
    if (cacheFile.exists()) {
      return getCachedHighlightsStream(cacheFile)
    }
    if (Constants.DEBUG) println("Cache file not found! Performing window search.")

    // Init some variables
    val sentences = new ListBuffer[Int]()

    // Cache the new results so that we can re-load it with an iterator
    val out = new java.io.FileWriter(cacheFile)
    var first = true

    // Read each annotation file and find the window-expanded highlight
    nlpDir.listFiles().foreach(nlpFile => {

      if (nlpFile.getName.endsWith(".txt")) {

        // Clear the list of found sentences
        sentences.clear()

        // Load CoreNLP annotation from file
        val nlpDoc = loadNLP(nlpFile)

        // Now the actual work, iterate over each sentence and find mentions of the activity
        for (i <- 0 until nlpDoc.sentences.size) {
          val s = nlpDoc.sentences(i)

          if (s.lemmas.isDefined && s.tags.isDefined) {
            // For each sentence, iterate over each token to see if the activity is in there.
            breakable {
              s.lemmas.get.zipWithIndex.foreach(l => {
                if (l._1.equalsIgnoreCase(activity) && NLPUtils.POS_VBs.contains(s.tags.get(l._2))) {
                  // If we found the activity in the correct POS, then add the surrounding range of sentences
                  val start = math.max(0, i - window)                             // inclusive
                  val end = math.min(nlpDoc.sentences.size - 1, i + window)       // inclusive
                  sentences.appendAll(start to end)

                  this.totalActivityInstances += 1
                }
              })
            } // end tokens loop

          }
        } // end sentences loop

        // If we found any highlight sentences, then write them to file.
        if (sentences.size > 0) {
          // Concatenate the the sentences
          val sb = new StringBuilder()
          collection.SortedSet(sentences: _*).foreach(i => {
            sb.append(nlpDoc.sentences(i).words.mkString(" ") + " " )

            this.totalExtendedSentences += 1
          })

          // Write to file
          if (first) {
            out.write(sb.toString().trim())
            first = false
          } else {
            out.write("\n" + sb.toString().trim())
          }
        }
      }

    }) // end loop
    out.close

    // Return the loaded file
    getCachedHighlightsStream(cacheFile)
  }


  /** Scan the neighborhood of the coref-extended highlights for the specified terms and
    * keep count of the number of highlights in which the terms appear in the correct
    * part-of-speech. Note for each highlight chain, we add _at most_ one to the count
    * if all terms appear in the sentence (even if they may appear multiple times).
    *
    * - retrieve coref-extended highlights
    * - run (tokenize + lemmatize + POS) on found passages
    * - filter for passages where the search terms match specific POS labels
    * - update frequency count for each valid match
    *
    * @param searchTerms List of other search terms and their associated POS restrictions
    *                              (set to None for no restrictions).
    * @param cacheFile Path to file containing the cached highlights.
    *
    * @return A count of the number of valid results.
    */
  def countHighlightsWithTermsPOS(searchTerms: List[(String, Option[Set[String]])], cacheFile: File): Long = {
    // Retrieve search highlights
    val highlights = getCachedHighlightsStream(cacheFile)

    // Lemmatize the search terms (but don't lemmatize the mental state, we want to use the JJ lemma).
    val searchLemmas = searchTerms.map(e => {
      if (dictionary.freq(0).contains(List(e._1)))
        (e._1.toLowerCase, e._2)
      else if (keywordsMap.contains(e._1))
        (keywordsMap.get(e._1).get.toLowerCase, e._2)
      else
        (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0).toLowerCase, e._2)
    }).toMap

    // The frequency count
    var frequency = 0L

    // Process highlights
    var doc: Document = null
    val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)
    highlights.foreach(h => {
      searchFlags.keys.foreach(k => searchFlags.put(k, false))

      // Iterate through each highlight and annotate
      doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(h))
      NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)

      // Filter for search terms appearing in the correct POS for _each_ highlight
      doc.sentences.foreach(s => {
        for (i <- 0 until s.lemmas.get.length) {
          if (s.tags.isDefined) {
            if (searchLemmas.contains(s.lemmas.get(i).toLowerCase)) {
              if (!searchLemmas.get(s.lemmas.get(i).toLowerCase).get.isDefined ||
                searchLemmas.get(s.lemmas.get(i).toLowerCase).get.get.contains(s.tags.get(i)))
                searchFlags.put(s.lemmas.get(i).toLowerCase, true)
            }
          }
        }
      })

      if (searchFlags.values.reduceLeft(_ && _)) {
        // If all search terms are found in the correct POS, then update the frequency count by 1.
        frequency += 1
      }

    })

    frequency
  }

  /** Compute the joint frequency counts for each activity-actor pair in combination with each
    * of the mental state.
    *
    * @param cacheFile The cached NLP-processed highlights file.
    * @param activity The activity.
    * @param actors The set of possible actors.
    * @param frequencyDir The frequency output directory for storing frequency counts.
    * @param targetPOSLabels The target labels of the mental states.
    * @param overwrite Overwrite all previously cached frequencies if True.
    */
  def computeStatesFrequencyAA(cacheFile: File, activity: (String, Option[Set[String]]),
                               actors: List[(String, Option[Set[String]])], frequencyDir: String,
                               targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                               overwrite: Boolean = false) {

    /** Helper function to write the frequency to file */
    def saveFrequency(searchTerms: List[String], freq: Long) {
      val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
      val writer = new java.io.FileWriter(file)
      writer.write(freq + "\n")
      writer.close()
    }

    // Retrieve search highlights
    val highlights = getCachedHighlightsStream(cacheFile).toArray

    // Lemmatize the search terms
    val activityLemma =
      (NLPUtils.annotateString(activity._1, POS = true, lemmatize = true).sentences(0).lemmas.get(0).toLowerCase,
        activity._2)
    val actorLemmas = actors.map(e => {
      (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0).toLowerCase, e._2)
    })

    // List of mental states
    val mentalStates = dictionary.getAllTokens().toList.map(_(0))

    // Iterate over each possible actor
    var searchTerms: List[String] = null
    var searchLemmas: scala.collection.Map[String, Option[Set[String]]] = null
    for (i <- -1 until actors.length) {
      // For each actor combination, let's reset the dictionary.
      dictionary.reset()

      // We will also count the number of extended highlights that properly contain all the search terms
      // (but may not necessarily contain any mental states).
      var freq = 0

      // Create a map of search terms
      if (i == -1) {
        searchTerms = List(activity._1.toLowerCase)
        searchLemmas = List(activityLemma).toMap
      } else {
        searchTerms = List(activity._1.toLowerCase) ::: List(actors(i)._1.toLowerCase)
        searchLemmas = (List(activityLemma) ::: List(actorLemmas(i))).toMap
      }

      // Skip this entire process if we already have the frequency counts for this particular search
      var shouldRun = false
      if (overwrite)
        shouldRun = true
      else {
        breakable {
          mentalStates.foreach(state => {
            val terms = searchTerms ::: List(state)
            val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (!file.exists()) {
              shouldRun = true
              break()
            }
          })
        }
        if (!shouldRun) {
          val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (searchTerms.size > 1 && !file.exists()) {
            shouldRun = true
          }
        }
      }

      // Only run the frequency counting process (which is very expensive) if we must
      if (shouldRun) {

        // Process highlights
        var doc: Document = null
        val targets = new ListBuffer[String]()
        val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)

        highlights.foreach(h => {
          // Reset book keeping, it's a new highlight
          targets.clear()
          searchFlags.keys.foreach(k => searchFlags.put(k, false))

          // Iterate through each highlight and annotate
          doc = processor.mkDocumentFromSentences(NLPUtils.dropLongSentences(h))
          NLPUtils.annotateCoreNLP(doc, POS = true, lemmatize = true)

          // Filter for search terms appearing in the correct POS for _each_ highlight
          doc.sentences.foreach(s => {
            for (i <- 0 until s.lemmas.get.length) {
              if (s.tags.isDefined) {
                if (targetPOSLabels.contains(s.tags.get(i)))
                  targets += s.lemmas.get(i).toLowerCase
                if (searchLemmas.contains(s.lemmas.get(i).toLowerCase)) {
                  if (!searchLemmas.get(s.lemmas.get(i).toLowerCase).get.isDefined ||
                    searchLemmas.get(s.lemmas.get(i).toLowerCase).get.get.contains(s.tags.get(i)))
                    searchFlags.put(s.lemmas.get(i).toLowerCase, true)
                }
              }
            }
          })

          if (searchFlags.values.reduceLeft(_ && _)) {
            // If all search terms are found in the correct POS, then update the frequency counts
            // for all keywords found in the highlight by one each.
            dictionary.increment(targets.toSet, None)
            freq += 1
          }

        })

        // Now dump out the dictionary frequency and save them.
        val freqResults = dictionary.freq(0).map(entry => (entry._1(0), entry._2.toLong))
        println("Search terms: " + searchTerms.mkString(" "))
        println("Search freqs: " + freq)
        println(freqResults.mkString(" "))
        freqResults.foreach(state => {
          val terms = searchTerms ::: List(state._1)
          val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (overwrite || !file.exists()) {
            saveFrequency(terms, state._2)
          }
        })

        // Also dump the frequency count of the search context
        val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
        if (searchTerms.size > 1 && (overwrite || !file.exists())) {
          saveFrequency(searchTerms, freq)
        }

      } // end if for running a frequency-counting process for a particular search-context

    } // end for loop over each actor

  }
}


/** NLP neighborhood singleton object */
object CorefNeighborhood {

  /** Construct and return a new Coref Neighborhood.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @return A new CorefNeighborhood model.
    */
  def createModel(statesDictionaryFile: String): CorefNeighborhood = {

    // Instantiate new model
    val model = new CorefNeighborhood()
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile))

    model
  }

}


/** Compute deleted interpolation joint frequency counts. */
object RunCorefComputeJointFrequency {

  def main(args: Array[String]) {

    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model
    val act = "hug"
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/frequency/nlp-coref"
    val nlpCachedFile = new File("/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/highlights/nlp/coref.txt")
    val targetPOSLabels = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs)

    // Location of annotations
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + act + "/xml/"
    val annotationSet = (1 to 45).map(annotationDir + act + "%02d".format(_) + ".xml")

    // Load annotation to create queries
    val queries = new ListBuffer[(String, String)]()
    annotationSet.foreach(annotationFile => {
      // Create a Word Neighborhood Model to generate queries
      val wnm = new WordNeighborhood(act, "")
      wnm.loadDetections(annotationFile)
      queries.appendAll(wnm.formulateQueriesAA())
    })
    val queriesWithPOS = queries.map(q => {
      List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
    }).toArray

    // Separate out the queries into activity and a set of actors
    val activity = queriesWithPOS.head(0)
    val actors = queriesWithPOS.map(q => q(1)).toSet.toList

    // Compute frequency
    val model = CorefNeighborhood.createModel(mentalStatesFile)
    model.computeStatesFrequencyAA(nlpCachedFile, activity, actors, frequencyDir, targetPOSLabels, overwrite = false)

  }

}


/** Run a demo to get extended highlights. */
object RunExtendedHighlights {

  /** Read in NLP annotations and output the extended highlights to a cached file. */
  def main(args: Array[String]) {

    val activity = "chase"

    // Instantiate the model
    val model = new CorefNeighborhood()

    // NLP & SwiRL directories
    val nlpDir = new File("/Volumes/MyPassport/data/text/corpora/Gigaword-" + activity + "-nlp")
    val cacheFile = new File("/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/nlp/coref-swirl.txt")

    // Coref highlights
    val swirlDir = Option(new File("/Volumes/MyPassport/data/text/corpora/Gigaword-" + activity + "-swirl"))
    model.getCorefHighlightsStream(activity, nlpDir, swirlDir, cacheFile)

    // Naive windowing highlights
    //model.getWindowHighlightsStream(activity, nlpDir, window = 1, cacheFile)

    println("\n\n***********************")
    println("Total number of " + activity + "s found: " + model.totalActivityInstances)
    println("Number of times NLP found both subject & object: " + model.totalSuccessfulNLPArgsParses)
    println("Number of times NLP failed & SwiRL succeeded: " + model.totalSuccessfulSwiRLArgsParses)
    println("Number of times NLP completely failed: " + model.totalFailureNLPArgsParses)
    println("Number of times NLP & SwiRL completely failed: " + model.totalFailureNLPAndSwiRLArgsParses)
    println("Number of extended sentences: " + model.totalExtendedSentences)

    if (swirlDir.isDefined)
      println("Number of bad SwiRL files: " + model.totalSwiRLParseErrors)

  }

}

/** Run a demo to get the args & coref of a document. */
object RunCorefNeighborhood {

  def main(args: Array[String]) {

    // The activity & text
    val activity = "chase"
    val text = "The police chased John around the corner.  John was very afraid."

    // Instantiate the model
    val model = new CorefNeighborhood()

    // Find coref chains
    Constants.DEBUG = true
    model.findArgsCorefChains(activity, text)

    println("\n\n***********************")
    println("Total number of " + activity + "s found: " + model.totalActivityInstances)
    println("Number of times NLP found both subject & object: " + model.totalSuccessfulNLPArgsParses)
    println("Number of times NLP failed & SwiRL succeeded: " + model.totalSuccessfulSwiRLArgsParses)
    println("Number of times NLP completely failed: " + model.totalFailureNLPArgsParses)
    println("Number of times NLP & SwiRL completely failed: " + model.totalFailureNLPAndSwiRLArgsParses)
    println("Number of extended sentences: " + model.totalExtendedSentences)

  }

}
