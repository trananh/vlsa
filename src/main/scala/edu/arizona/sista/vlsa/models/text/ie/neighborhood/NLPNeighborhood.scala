package edu.arizona.sista.vlsa.models.text.ie.neighborhood

import edu.arizona.sista.processors.struct.DirectedGraphEdgeIterator
import edu.arizona.sista.processors.{Document, Sentence, CorefMention}
import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.models.data.SwiRLDocument
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode.ActorMode
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import edu.arizona.sista.vlsa.utils.{Constants, NLPUtils}
import java.io.File
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.{SortedSet, mutable}
import scala.util.control.Breaks._

/** A word-search model to find the occurrence frequency of keywords within some neighborhood
  * of some search activity.  This model operates specifically on results retrieved from an index
  * corpus.  It improves over the simple DocNeighborhood model by employing different
  * NLP techniques to seek out "valid" occurrences. Among the different techniques used are:
  *
  *  - Using dependency relations and semantic roles to identify the arguments of the activity
  *  - Using coref resolution to find other mentions of the arguments (aka actors)
  *  - Using syntactic dependencies to find occurrences of keywords that specifically
  *     reference the actor(s) of interest
  *
  * @constructor An NLP word-neighborhood model that operates on an indexed corpus.
  *
  * @author trananh
  */
class NLPNeighborhood extends CorefNeighborhood {

  /** Search the list of syntactic dependencies for the sentence to find the set of adjective complements
    * that are related (via some predicates) to the headwords that reference the subject (actor) of interest.
    *
    * @param sentence The NLP annotated sentence to search within.
    * @param actorHeadIndex Headword indices in the sentence that reference the subject (actor) of interest.
    *
    * @return Set of headword indices of adjective complements in the sentence that relate to the
    *         subject (actor) of interest.
    */
  def getComplementsForActor(sentence: Sentence, actorHeadIndex: Set[Int]): Set[Int] = {
    // List of targets
    val targets = new ListBuffer[Int]()

    // The dependencies involving the subject of interest as the modifier, that is
    // the person of interest is the subject of a verb.
    val feelHeadVerbIdx = new ListBuffer[Int]()

    // Iterate over the list of dependencies for the sentence and find all dependencies
    // related to the headwords of interest.
    var iterator = new DirectedGraphEdgeIterator[String](sentence.dependencies.get)
    while (iterator.hasNext) {
      val d = iterator.next

      /* Pattern: "He chased the angry suspect."
       *  => head:4 modifier:3 label:amod
       *
       * Also applies for:
       *  - A depressed suspect.
       *  - A welcoming suspect.
       */
      if (actorHeadIndex.contains(d._1) && d._3.equalsIgnoreCase("amod")) {
        targets.append(d._2)
      }

      // We are also interested in dependencies where the actor of interest is the subject of the verb.
      if (actorHeadIndex.contains(d._2) && (d._3.equalsIgnoreCase("nsubj") || d._3.equalsIgnoreCase("nsubjpass"))) {

        /* For the following verbs, we'll need to further find the complements later.  Else
         * the verb itself may be a mis-tagged adjective state.
         *  - feel
         */
        if ("feel".equals(sentence.lemmas.get(d._1).toLowerCase)) {

          feelHeadVerbIdx.append(d._1)

        } else {

          /* Pattern: "I am sad."
           *  => head:2 modifier:0 label:nsubj
           *  => head:2 modifier:1 label:cop
           *
           * Also applies for:
           *  - He was super happy.
           *  - We are terrific.
           *  - They were very angry.
           */

          /* Pattern: "He was determined."
           *  => head:2 modifier:0 label:nsubjpass
           *  => head:2 modifier:1 label:auxpass
           *
           * Also applies for:
           *  - He is agitated.
           *  - They are overjoyed.
           *  - He was depressed.
           */

          /* Pattern: "She is welcoming."
           *  => head:2 modifier:0 label:nsubj
           *  => head:2 modifier:1 label:aux
           */

          targets.append(d._1)

        }
      }
    }

    // Iterate through all dependencies again and find complements of the verbs.
    val headVerbsSet = feelHeadVerbIdx.toSet
    if (headVerbsSet.size > 0) {
      iterator = new DirectedGraphEdgeIterator[String](sentence.dependencies.get)
      while (iterator.hasNext) {
        val d = iterator.next

        // The dependency must have the verb as the head
        if (headVerbsSet.contains(d._1)) {

          if (d._3.equalsIgnoreCase("acomp")) {

            /* Pattern: "He felt ecstatic."
             *  => head:1 modifier:0 label:nsubj
             *  => head:1 modifier:2 label:acomp
             *
             * Also applies for:
             *  - They feel very overwhelmed.
             *  - She felt determined.
             */

            targets.append(d._2)

          } else if (d._3.equalsIgnoreCase("dep")) {

            /* Pattern: "We feel trusted."
             *  => head:1 modifier:0 label:nsubj
             *  => head:1 modifier:2 label:dep
             *
             * Also applies for:
             *  - I felt determined to win.
             */

            targets.append(d._2)

          }

        }
      }

    }

    // Return the headword indices as a set (no repeats)
    targets.toSet
  }

  /** Validate whether or not all search terms appear in the highlighted sentences under
    * the correct POS tagging.
    *
    * @param nlpDoc The NLP annotation for the document.
    * @param highlightSentences Set of highlighted sentences, specified by their indices.
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    *
    * @return True if all search terms appear in the highlighted sentences under the correct
    *         POS tagging.
    */
  def validateSearchTermsWithPOS(nlpDoc: Document, highlightSentences: SortedSet[Int],
                                 searchTerms: List[(String, Option[Set[String]])]): Boolean = {

    // Lemmatize the search terms (but don't lemmatize the mental state, we want to use the JJ lemma).
    val searchLemmas = searchTerms.map(e => {
      if (dictionary.freq(0).contains(List(e._1)))
        (e._1.toLowerCase, e._2)
      else if (keywordsMap.contains(e._1))
        (keywordsMap.get(e._1).get.toLowerCase, e._2)
      else
        (NLPUtils.annotateString(e._1.trim, POS = true, lemmatize = true).sentences(0).lemmas.get(0).toLowerCase, e._2)
    }).toMap

    // Process highlights
    val searchFlags = mutable.Map(searchLemmas.map(e => (e._1, false)).toSeq: _*)

    // Filter for search terms appearing in the correct POS in the highlights
    nlpDoc.sentences.zipWithIndex.foreach(s => {
      if (highlightSentences.contains(s._2)) {
        for (i <- 0 until s._1.lemmas.get.length) {
          if (s._1.tags.isDefined) {
            if (searchLemmas.contains(s._1.lemmas.get(i).toLowerCase)) {
              if (!searchLemmas.get(s._1.lemmas.get(i).toLowerCase).get.isDefined ||
                searchLemmas.get(s._1.lemmas.get(i).toLowerCase).get.get.contains(s._1.tags.get(i)))
                searchFlags.put(s._1.lemmas.get(i).toLowerCase, true)
            }
          }
        }
      }
    })

    searchFlags.values.reduceLeft(_ && _)
  }

  /** Scan the sentences in the coref-chains highlight and keep count of keywords from the dictionary.
    * The procedure is as follows:
    *
    * - ensure all search terms appear in the coref-chains highlight
    * - if the highlight is valid (i.e., contains all search terms) then
    *     * search for adjective complements in the highlight that reference the main actor(s) of the chains
    *     * update count for keyword tokens found
    *
    * @param nlpDoc The NLP annotation for the document.
    * @param corefChains The coref-chains that form the highlight.
    * @param activity The activity.
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param targetPOSLabels Set of POS labels used to filter the tokens.
    * @param dictionary The dictionary use to keep count of keyword tokens, defaults to the main class dictionary.
    *
    * @return True if all search terms appear in the coref-chains under the correct POS tagging.
    */
  def processCorefChainsWithTermsPOS(nlpDoc: Document,
                                     corefChains: List[Array[CorefMention]],
                                     activity: (String, Option[Set[String]]),
                                     searchTerms: List[(String, Option[Set[String]])],
                                     targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                                     dictionary: FrequencyDictionary[String] = this.dictionary): Boolean = {

    // Convert the coref chains to a sentence mapping, which maps each to the headwords
    // of the actor of interest in that sentence.
    val sentMap = new HashMap[Int, ListBuffer[Int]]()
    corefChains.foreach(chain => {
      chain.foreach(cm => {
        if (!sentMap.contains(cm.sentenceIndex))
          sentMap.put(cm.sentenceIndex, new ListBuffer[Int]())
        sentMap.get(cm.sentenceIndex).get.append(cm.headIndex)
      })
    })

    // Let's also keep track of how many coref chains contain all search terms
    var valid = false

    // Only process the highlight if it contains all the necessary search terms.
    val highlightSentences = collection.SortedSet(sentMap.keys.toList: _*)
    val search = (activity :: searchTerms)
    if (validateSearchTermsWithPOS(nlpDoc, highlightSentences, search)) {

      // A valid coref chain.
      valid = true

      // A valid highlight. For each highlight sentence, look for mental state dependencies.
      highlightSentences.foreach(i => {
        val sentence = nlpDoc.sentences(i)

        // Get all dependency targets related to the subject (actor) of interest
        val targets = getComplementsForActor(sentence, sentMap.get(i).get.toSet)

        // Filter for targets of the correct POS tag
        val posFiltered = targets.filter(j => targetPOSLabels.contains(sentence.tags.get(j)))

        // If all search terms & the targets are found in the correct POS then update the frequency counts
        // for all keywords found in the highlight by one each.
        dictionary.increment(posFiltered.map(j => sentence.lemmas.get(j)), None)

      })

    }

    valid
  }

  /** Scan the sentences in the coref-chains highlight that reference the actor of interest and
    * keep count of keywords from the dictionary. The procedure is as follows:
    *
    * - for each relevant document in the corpus:
    * - ensure all search terms appear in the coref-chains highlight
    * - if the highlight is valid (i.e., contains all search terms) then
    *     * search for adjective complements in the highlight that reference the main actor(s) of the chains
    *     * update count for keyword tokens found
    *
    * @param activity The activity.
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param nlpDir Directory containing cached NLP annotations.
    * @param swirlDir Optional directory containing cached SwiRL annotations (default to None).
    * @param targetPOSLabels Set of POS labels used to filter the tokens.
    * @param mode Specifies which actor(s) to process mental states for
    *             (defaults to ActorMode.All, which means process mental states for all actors).
    *
    * @return The number of documents with coref-chain highlights that contain all search terms
    *         under the correct POS tagging.
    */
  def processWithTermsPOS(activity: (String, Option[Set[String]]),
                          searchTerms: List[(String, Option[Set[String]])],
                          nlpDir: File, swirlDir: Option[File] = None,
                          targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                          mode: ActorMode = ActorMode.All): Long = {
    // Init some variables
    var corefChains: (List[Array[CorefMention]], List[Array[CorefMention]]) = null

    var frequency = 0
    var validHighlight = false

    // Read each annotation file and find the coref-chains
    nlpDir.listFiles().foreach(nlpFile => {

      if (nlpFile.getName.endsWith(".txt")) {

        if (Constants.DEBUG) println("\n\nProcessing document: " + nlpFile.getName)
        validHighlight = false

        // Load CoreNLP annotation from file
        val nlpDoc = loadNLP(nlpFile)

        // Load SwiRL annotation if exists
        var swirlDoc: Option[SwiRLDocument] = None
        if (swirlDir.isDefined && swirlDir.get.exists()) {
          swirlDoc = loadSwiRL(Option(new File(swirlDir.get.getAbsolutePath + "/" + nlpFile.getName)))
        }

        try {

          // Find the coref chains for the subjects and objects
          corefChains = findArgsCorefChains(activity._1, nlpDoc, swirlDoc)
          val subChains = corefChains._1
          val objChains = corefChains._2

          // Process the coref-chains highlight (based on mode)
          mode match {
            case ActorMode.Subject => validHighlight =
                processCorefChainsWithTermsPOS(nlpDoc, subChains, activity, searchTerms, targetPOSLabels)

            case ActorMode.Object => validHighlight =
              processCorefChainsWithTermsPOS(nlpDoc, objChains, activity, searchTerms, targetPOSLabels)

            case ActorMode.All => validHighlight =
              processCorefChainsWithTermsPOS(nlpDoc, (subChains ++ objChains), activity, searchTerms, targetPOSLabels)

            case _ => throw new Exception("Unrecognized mode.")
          }

          if (validHighlight)
            frequency += 1

        } catch {
          case e: Exception => println("ERROR: " + nlpFile.getName + "\n" + e.getStackTrace.toString)
        }
      }

    })

    frequency
  }

  /** Compute the joint frequency counts for each activity-actor pair in combination with each
    * of the mental state.
    *
    * @param nlpDir Directory containing cached NLP annotations.
    * @param swirlDir Optional directory containing cached SwiRL annotations (default to None).
    * @param activity The activity.
    * @param actors The set of possible actors.
    * @param frequencyDir The frequency output directory for storing frequency counts.
    * @param targetPOSLabels The target labels of the mental states.
    * @param mode Specifies which actor(s) to process mental states for
    *             (defaults to ActorMode.All, which means process mental states for all actors).
    * @param overwrite Overwrite all previously cached frequencies if True.
    */
  def computeNLPStatesFrequencyAA(nlpDir: File, swirlDir: Option[File] = None, activity: (String, Option[Set[String]]),
                                  actors: List[(String, Option[Set[String]])], frequencyDir: String,
                                  targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                                  mode: ActorMode = ActorMode.All,
                                  overwrite: Boolean = false) {

    /** Helper function to write the frequency to file */
    def saveFrequency(searchTerms: List[String], freq: Long) {
      val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
      val writer = new java.io.FileWriter(file)
      writer.write(freq + "\n")
      writer.close()
    }

    // List of mental states
    val mentalStates = dictionary.getAllTokens().toList.map(_(0))

    // Iterate over each possible actor
    var searchTerms: List[(String, Option[Set[String]])] = null
    for (i <- -1 until actors.length) {
      // For each actor combination, let's reset the dictionary.
      dictionary.reset()

      // Create a map of search terms
      if (i == -1) {
        searchTerms = List((activity._1.toLowerCase, activity._2))
      } else {
        searchTerms = List((activity._1.toLowerCase, activity._2)) ::: List((actors(i)._1.toLowerCase, actors(i)._2))
      }

      // Skip this entire process if we already have the frequency counts for this particular search
      var shouldRun = false
      if (overwrite)
        shouldRun = true
      else {
        breakable {
          mentalStates.foreach(state => {
            val terms = searchTerms.map(_._1) ::: List(state)
            val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (!file.exists()) {
              shouldRun = true
              break()
            }
          })
        }
        if (!shouldRun) {
          val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (searchTerms.size > 1 && !file.exists()) {
            shouldRun = true
          }
        }
      }

      // Only run the frequency counting process (which is very expensive) if we must
      if (shouldRun) {

        // Process highlights
        val f = processWithTermsPOS(activity = searchTerms.head, searchTerms.slice(1, searchTerms.length),
          nlpDir = nlpDir, swirlDir = swirlDir, targetPOSLabels, mode)

        // Now dump out the dictionary frequency and save them.
        val freqResults = dictionary.freq(0).map(entry => (entry._1(0), entry._2.toLong))
        println("Search terms: " + searchTerms.map(_._1).mkString(" "))
        println("Search freqs: " + f)
        println(freqResults.mkString(" "))
        freqResults.foreach(state => {
          val terms = searchTerms.map(_._1) ::: List(state._1)
          val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (overwrite || !file.exists()) {
            saveFrequency(terms, state._2)
          }
        })

        // Also dump the frequency count of the search context
        val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
        if (searchTerms.size > 1 && (overwrite || !file.exists())) {
          saveFrequency(searchTerms.map(_._1), f)
        }

      } // end if for running a frequency-counting process for a particular search-context

    } // end for loop over each actor

  }

  /** Compute the joint frequency counts for each activity-actor-location triplet in combination with each
    * of the mental state.
    *
    * @param nlpDir Directory containing cached NLP annotations.
    * @param swirlDir Optional directory containing cached SwiRL annotations (default to None).
    * @param activity The activity.
    * @param actors The set of possible actors.
    * @param locations The set of possible locations.
    * @param frequencyDir The frequency output directory for storing frequency counts.
    * @param targetPOSLabels The target labels of the mental states.
    * @param mode Specifies which actor(s) to process mental states for
    *             (defaults to ActorMode.All, which means process mental states for all actors).
    * @param overwrite Overwrite all previously cached frequencies if True.
    */
  def computeNLPStatesFrequencyAAL(nlpDir: File, swirlDir: Option[File] = None, activity: (String, Option[Set[String]]),
                                   actors: List[(String, Option[Set[String]])],
                                   locations: List[(String, Option[Set[String]])], frequencyDir: String,
                                   targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                                   mode: ActorMode = ActorMode.All,
                                   overwrite: Boolean = false) {

    /** Helper function to write the frequency to file */
    def saveFrequency(searchTerms: List[String], freq: Long) {
      val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
      val writer = new java.io.FileWriter(file)
      writer.write(freq + "\n")
      writer.close()
    }

    // List of mental states
    val mentalStates = dictionary.getAllTokens().toList.map(_(0))

    // The search terms
    var searchTerms: List[(String, Option[Set[String]])] = null

    // Iterate over each possible actor
    for (aIdx <- 0 until actors.length) {

      // Iterate over each location
      for (lIdx <- 0 until locations.length) {

        // For each actor-location combination, let's reset the dictionary.
        dictionary.reset()

        // Create a list of search terms following the AAL pattern
        searchTerms = List((activity._1.toLowerCase, activity._2), (actors(aIdx)._1.toLowerCase, actors(aIdx)._2),
          (locations(lIdx)._1.toLowerCase, locations(lIdx)._2))

        // Skip this entire process if we already have the frequency counts for this particular search
        var shouldRun = false
        if (overwrite)
          shouldRun = true
        else {
          breakable {
            mentalStates.foreach(state => {
              val terms = searchTerms.map(_._1) ::: List(state)
              val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
              if (!file.exists()) {
                shouldRun = true
                break()
              }
            })
          }
          if (!shouldRun) {
            val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (searchTerms.size > 1 && !file.exists()) {
              shouldRun = true
            }
          }
        }

        // Only run the frequency counting process (which is very expensive) if we must
        if (shouldRun) {

          // Process highlights
          val f = processWithTermsPOS(activity = searchTerms.head, searchTerms.slice(1, searchTerms.length),
            nlpDir = nlpDir, swirlDir = swirlDir, targetPOSLabels, mode)

          // Now dump out the dictionary frequency and save them.
          val freqResults = dictionary.freq(0).map(entry => (entry._1(0), entry._2.toLong))
          println("Search terms: " + searchTerms.map(_._1).mkString(" "))
          println("Search freqs: " + f)
          println(freqResults.mkString(" "))
          freqResults.foreach(state => {
            val terms = searchTerms.map(_._1) ::: List(state._1)
            val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (overwrite || !file.exists()) {
              saveFrequency(terms, state._2)
            }
          })

          // Also dump the frequency count of the search context
          val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (searchTerms.size > 1 && (overwrite || !file.exists())) {
            saveFrequency(searchTerms.map(_._1), f)
          }

        } // end if for running a frequency-counting process for a particular search-context

      } // end for loop for each location

    } // end for loop over each actor

  }

  /** Compute the joint frequency counts for each activity-actor-relationship triplet in combination with each
    * of the mental state.
    *
    * @param nlpDir Directory containing cached NLP annotations.
    * @param swirlDir Optional directory containing cached SwiRL annotations (default to None).
    * @param activity The activity.
    * @param actors The set of possible actors.
    * @param relationships The set of possible relationships.
    * @param frequencyDir The frequency output directory for storing frequency counts.
    * @param targetPOSLabels The target labels of the mental states.
    * @param mode Specifies which actor(s) to process mental states for
    *             (defaults to ActorMode.All, which means process mental states for all actors).
    * @param overwrite Overwrite all previously cached frequencies if True.
    */
  def computeNLPStatesFrequencyAAR(nlpDir: File, swirlDir: Option[File] = None, activity: (String, Option[Set[String]]),
                                   actors: List[(String, Option[Set[String]])],
                                   relationships: List[(String, Option[Set[String]])], frequencyDir: String,
                                   targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs),
                                   mode: ActorMode = ActorMode.All,
                                   overwrite: Boolean = false) {

    /** Helper function to write the frequency to file */
    def saveFrequency(searchTerms: List[String], freq: Long) {
      val file = new File(frequencyDir + "/" + searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
      val writer = new java.io.FileWriter(file)
      writer.write(freq + "\n")
      writer.close()
    }

    // List of mental states
    val mentalStates = dictionary.getAllTokens().toList.map(_(0))

    // The search terms
    var searchTerms: List[(String, Option[Set[String]])] = null

    // Iterate over each possible actor
    for (aIdx <- 0 until actors.length) {

      // Iterate over each relationship
      for (rIdx <- 0 until relationships.length) {

        // For each actor-relationship combination, let's reset the dictionary.
        dictionary.reset()

        // Create a list of search terms following the AAR pattern
        searchTerms = List((activity._1.toLowerCase, activity._2), (actors(aIdx)._1.toLowerCase, actors(aIdx)._2),
          (relationships(rIdx)._1.toLowerCase, relationships(rIdx)._2))

        // Skip this entire process if we already have the frequency counts for this particular search
        var shouldRun = false
        if (overwrite)
          shouldRun = true
        else {
          breakable {
            mentalStates.foreach(state => {
              val terms = searchTerms.map(_._1) ::: List(state)
              val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
              if (!file.exists()) {
                shouldRun = true
                break()
              }
            })
          }
          if (!shouldRun) {
            val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (searchTerms.size > 1 && !file.exists()) {
              shouldRun = true
            }
          }
        }

        // Only run the frequency counting process (which is very expensive) if we must
        if (shouldRun) {

          // Process highlights
          val f = processWithTermsPOS(activity = searchTerms.head, searchTerms.slice(1, searchTerms.length),
            nlpDir = nlpDir, swirlDir = swirlDir, targetPOSLabels, mode)

          // Now dump out the dictionary frequency and save them.
          val freqResults = dictionary.freq(0).map(entry => (entry._1(0), entry._2.toLong))
          println("Search terms: " + searchTerms.map(_._1).mkString(" "))
          println("Search freqs: " + f)
          println(freqResults.mkString(" "))
          freqResults.foreach(state => {
            val terms = searchTerms.map(_._1) ::: List(state._1)
            val file = new File(frequencyDir + "/" + terms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt")
            if (overwrite || !file.exists()) {
              saveFrequency(terms, state._2)
            }
          })

          // Also dump the frequency count of the search context
          val file = new File(frequencyDir + "/" + searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt")
          if (searchTerms.size > 1 && (overwrite || !file.exists())) {
            saveFrequency(searchTerms.map(_._1), f)
          }

        } // end if for running a frequency-counting process for a particular search-context

      } // end for loop for each relationship

    } // end for loop over each actor

  }

}


/** NLP neighborhood singleton object */
object NLPNeighborhood {

  /** Different grouping modes for the NLP neighborhood model.
    *   - Subject: Process mental states only for the subject subject (of the activity).
    *   - Object: Process mental states only for the object actor (of the activity).
    *   - All: Process mental states for all actors (of the activity).
    */
  object ActorMode extends Enumeration {
    type ActorMode = Value
    val Subject, Object, All = Value
  }

  /** Construct and return a new NLP Neighborhood.
    * @param statesDictionaryFile Dictionary file containing list of mental states.
    * @return A new NLPNeighborhood model.
    */
  def createModel(statesDictionaryFile: String): NLPNeighborhood = {

    // Instantiate new model
    val model = new NLPNeighborhood()
    if (statesDictionaryFile.length > 0)
      model.loadAdjectiveDictionary(new File(statesDictionaryFile))

    model
  }

}


/** Compute deleted interpolation joint frequency counts. */
object RunNLPComputeJointFrequency {

  def main(args: Array[String]) {

    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the model
    val act = "hug"
    val mentalStatesFile = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val nlpDir = new File("/Volumes/MyPassport/data/text/corpora/Gigaword-" + act + "-nlp")
    val swirlDir = None
    val frequencyDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + act + "/frequency/nlp"
    val targetPOSLabels = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs)
    val mode = ActorMode.All

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
    val model = NLPNeighborhood.createModel(mentalStatesFile)
    model.computeNLPStatesFrequencyAA(nlpDir, swirlDir, activity, actors,
      frequencyDir, targetPOSLabels, mode, overwrite = false)

  }

}

/** Run a demo to get the args, coref, and complements of a document. */
object RunNLPNeighborhood {

  /** Convert the coref chains to a sentence mapping, which maps each to the headwords
    * of the actor of interest in that sentence.
    */
  def mapChains(corefChains: List[Array[CorefMention]]): HashMap[Int, ListBuffer[Int]] = {
    val sentMap = new HashMap[Int, ListBuffer[Int]]()
    corefChains.foreach(chain => {
      chain.foreach(cm => {
        if (!sentMap.contains(cm.sentenceIndex))
          sentMap.put(cm.sentenceIndex, new ListBuffer[Int]())
        sentMap.get(cm.sentenceIndex).get.append(cm.headIndex)
      })
    })
    sentMap
  }

  def main(args: Array[String]) {

    // The activity & text
    val activity = "chase"
    val text = "The police chased John around the corner.  John was very afraid."

    // Instantiate the model
    val model = new NLPNeighborhood()
    model.loadAdjectiveDictionary(
      new File("/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"))

    // Annotate doc
    val doc = model.processor.annotate(text)

    // Find coref chains
    Constants.DEBUG = true
    val (subj, obj) = model.findArgsCorefChains(activity, doc)

    if (Constants.DEBUG) println("\n\n==> Step 3: COMPLEMENTS EXTRACTION")
    if (Constants.DEBUG) println("\nComplement(s) for SUBJECTS:")
    val subjMap = mapChains(subj)
    val subjSentences = collection.SortedSet(subjMap.keys.toList: _*)
    subjSentences.foreach(sentenceIndex => {
      val comps = model.getComplementsForActor(doc.sentences(sentenceIndex), subjMap.get(sentenceIndex).get.toSet)
      val filtered = comps.filter(i =>
        model.dictionary.freq(0).contains(List(doc.sentences(sentenceIndex).lemmas.get(i))))
      print("\tsentence (" + sentenceIndex + "): ")
      if (filtered.size > 0) println(filtered.map(doc.sentences(sentenceIndex).words(_)).mkString(", "))
      else println("NONE")
    })

    if (Constants.DEBUG) println("\nComplement(s) for OBJECT:")
    val objMap = mapChains(obj)
    val objSentences = collection.SortedSet(objMap.keys.toList: _*)
    objSentences.foreach(sentenceIndex => {
      val comps = model.getComplementsForActor(doc.sentences(sentenceIndex), objMap.get(sentenceIndex).get.toSet)
      val filtered = comps.filter(i =>
        model.dictionary.freq(0).contains(List(doc.sentences(sentenceIndex).lemmas.get(i))))
      print("\tsentence (" + sentenceIndex + "): ")
      if (filtered.size > 0) println(filtered.map(doc.sentences(sentenceIndex).words(_)).mkString(", "))
      else println("NONE")
    })


  }

}
