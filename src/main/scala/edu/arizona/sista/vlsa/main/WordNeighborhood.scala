package edu.arizona.sista.vlsa.main

import com.google.gson.Gson
import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.main.WordNeighborhood.ModelTypes
import edu.arizona.sista.vlsa.main.WordNeighborhood.ModelTypes.ModelTypes
import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles
import edu.arizona.sista.vlsa.models.data.{DetectionsAnnotation, VideoAnnotation}
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.{WebNeighborhood, VecNeighborhood, DocNeighborhood}
import edu.arizona.sista.vlsa.models.text.ie.{NLPDeletedInterpolation, HighlightsDeletedInterpolation, BackOffLinearInterpolation, DeletedInterpolation}
import edu.arizona.sista.vlsa.search.lucene.IndexSearcher
import edu.arizona.sista.vlsa.search.web.BingSearcher
import edu.arizona.sista.vlsa.utils.NLPUtils
import java.io.File
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An information extraction model that makes use of several underlying models, such as the
  * documents-based back-off model, vector-based back-off model, and documents-based deleted
  * interpolation model in order to extract distributions of mental states from text to describe video
  * scenes.
  *
  * @constructor An information-extraction application that uses the word-neighborhood models.
  * @param activity The main activity.
  * @param statesDictionary Path to file containing mental state terms.
  * @param distCacheDirectory Directory used for caching intermediate distribution results from
  *                           back-off linear interpolation models.
  *
  * @author trananh
  */
class WordNeighborhood(var activity: String, val statesDictionary: String,
                       var distCacheDirectory: Option[String] = None) {

  /** Detection labels discovered from the video. */
  val detections: DetectionsAnnotation = new DetectionsAnnotation()

  /** Information extraction models that uses the word-neighborhood approach. */
  var delNLPModel: Option[NLPDeletedInterpolation] = None
  var delCorefModel: Option[HighlightsDeletedInterpolation] = None
  var delDocModel: Option[DeletedInterpolation] = None
  var docModel: Option[DocNeighborhood] = None
  var vecModel: Option[VecNeighborhood] = None
  var webModel: Option[WebNeighborhood] = None
  var baselineModel: Option[WebNeighborhood] = None

  /** Extra information for each model */
  var docModelDataDir: Option[String] = None
  var webModelDataDir: String = ""

  /** Flag to overwrite cached results or not */
  var overwriteCache: Boolean = true
  if (distCacheDirectory.isDefined) {
    overwriteCache = false
  }

  /** Load video detections from annotation file.
    * @param annotationFile Path to file containing detections annotation.
    */
  def loadDetections(annotationFile: String) {
    val annotation = VideoAnnotation.fromXML(annotationFile)
    annotation.detections.values.foreach(d => detections.addDetections(d.getAllDetections(): _*))
    activity = annotation.activity
  }

  /** Load video detections from annotation file for the subject (of the activity).
    * @param annotationFile Path to file containing detections annotation.
    */
  def loadSubjectDetections(annotationFile: String) {
    val annotation = VideoAnnotation.fromXML(annotationFile)
    annotation.detections.foreach(kv => {
      if (Roles.Object != kv._1) {
        detections.addDetections(kv._2.getAllDetections(): _*)
      }
    })
    activity = annotation.activity
  }

  /** Load video detections from annotation file for the object (of the activity).
    * @param annotationFile Path to file containing detections annotation.
    */
  def loadObjectDetections(annotationFile: String) {
    val annotation = VideoAnnotation.fromXML(annotationFile)
    annotation.detections.foreach(kv => {
      if (Roles.Subject != kv._1) {
        detections.addDetections(kv._2.getAllDetections(): _*)
      }
    })
    activity = annotation.activity
  }

  /** Initialize the NLP Deleted Interpolation model.
    *
    * @param indexDir Path to the corpus index.
    * @param frequencyDir Path to the directory containing the cached frequency data.
    * @param highlightsDir Path to the directory containing the cached highlights for docs.
    */
  def initDelNLPNeighborhood(indexDir: String, frequencyDir: String, highlightsDir: String){
    delNLPModel = Option(new NLPDeletedInterpolation(statesDictionary, indexDir, frequencyDir, highlightsDir))
  }

  /** Initialize the Coref Deleted Interpolation model.
    *
    * @param indexDir Path to the corpus index.
    * @param frequencyDir Path to the directory containing the cached frequency data.
    * @param highlightsDir Path to the directory containing the cached highlights for docs.
    * @param nlpFile Path to the cached NLP highlights file.
    */
  def initDelCorefNeighborhood(indexDir: String, frequencyDir: String, highlightsDir: String, nlpFile: String){
    delCorefModel = Option(new HighlightsDeletedInterpolation(statesDictionary, indexDir, frequencyDir, highlightsDir,
      new File(nlpFile)))
  }

  /** Initialize the Deleted Interpolation model.
    *
    * @param indexDir Path to the corpus index.
    * @param frequencyDir Path to the directory containing the cached frequency data.
    * @param highlightsDir Path to the directory containing the cached highlights.
    */
  def initDelDocNeighborhood(indexDir: String, frequencyDir: String, highlightsDir: String) {
    delDocModel = Option(new DeletedInterpolation(statesDictionary, indexDir, frequencyDir, highlightsDir))
  }

  /** Initialize the Document Neighborhood model.
    * @param indexDir Path to the corpus index.
    * @param dataDir Path to directory containing the cached highlights from previous searches,
    *                which can significantly speed up computation time (defaults to None for no
    *                cache data dir).
    */
  def initDocNeighborhood(indexDir: String, dataDir: Option[String] = None) {
    docModel = Option(DocNeighborhood.createModel(statesDictionary, indexDir))
    docModelDataDir = dataDir
  }

  /** Initialize the Vector Neighborhood model.
    * @param vecFile Path to the binary file containing the trained word2vec model.
    */
  def initVecNeighborhood(vecFile: String) {
    vecModel = Option(VecNeighborhood.createModel(statesDictionary, vecFile))
  }

  /** Initialize the Web Neighborhood model.
    * @param dataDir Path to directory containing search results cached from the web.
    */
  def initWebNeighborhood(dataDir: String) {
    webModel = Option(WebNeighborhood.createModel(statesDictionary))
    webModelDataDir = dataDir
  }

  /** Initialize the baseline model. */
  def initBaseline() {
    baselineModel = Option(new WebNeighborhood(new BingSearcher()))
    baselineModel.get.loadAdjectiveDictionary(new File(statesDictionary),
      defaultScore = WebNeighborhood.DEFAULT_SCORE + 1.0)
  }

  /** Clear the model and free any memory usage. */
  def clearDelNLPNeighborhood() {
    delNLPModel = None
  }

  /** Clear the model and free any memory usage. */
  def clearDelCorefNeighborhood() {
    delCorefModel = None
  }

  /** Clear the model and free any memory usage. */
  def clearDelDocNeighborhood() {
    delDocModel = None
  }

  /** Clear the model and free any memory usage. */
  def clearDocNeighborhood() {
    docModel.get.clear()
    docModel = None
    docModelDataDir = None
  }

  /** Clear the model and free any memory usage. */
  def clearVecNeighborhood() {
    vecModel.get.clear()
    vecModel = None
  }

  /** Clear the model and free any memory usage. */
  def clearWebNeighborhood() {
    webModel.get.clear()
    webModel = None
    webModelDataDir = ""
  }

  /** Clear the model and free any memory usage. */
  def clearBaseline() {
    baselineModel.get.clear()
    baselineModel = None
  }

  /** Read a cached JSON scores map from file.
    *
    * @param file File to read from.
    * @return Scores read from file.
    */
  def readCacheScores(file: File): mutable.Map[String, Double] = {
    val gson = new Gson()
    val source = scala.io.Source.fromFile(file)
    val line = source.getLines().next()
    val scores = gson.fromJson[java.util.Map[String, Double]](line, classOf[java.util.Map[String, Double]])
    source.close()
    return scores
  }

  /** Write a scores map to file using JSON.
    *
    * @param file File to write to.
    */
  def writeCacheScores(file: File, scores: java.util.Map[String, Double]) {
    val gson = new Gson()
    val writer = new java.io.FileWriter(file)
    writer.write(gson.toJson(scores) + "\n")
    writer.close()
  }

  /** Clear all cached results */
  def clearCache() {
    if (distCacheDirectory.isDefined) {
      for {
        files <- Option(new File(distCacheDirectory.get + "/doc").listFiles)
        file <- files if file.getName.endsWith(".txt")
      } file.delete()
      for {
        files <- Option(new File(distCacheDirectory.get + "/vec").listFiles)
        file <- files if file.getName.endsWith(".txt")
      } file.delete()
      for {
        files <- Option(new File(distCacheDirectory.get + "/web").listFiles)
        file <- files if file.getName.endsWith(".txt")
      } file.delete()
    }
  }

  /** Prune the given distribution by the top probability mass, which keeps the part of the
    * distribution that totals alpha (0-1) percent of the entire distribution.
    * @param scores The distribution.
    * @param alpha Percentage of the top probability mass to preserve.
    * @return The pruned distribution.
    */
  def pruneByProbMass(scores: List[(String, Double)], alpha: Double): List[(String, Double)] = {
    val sorted = scores.sortWith(_._2 > _._2)
    var mass = 0.0
    var i = 0
    while (mass < alpha && i < sorted.size) {
      mass += sorted(i)._2
      i += 1
    }
    sorted.slice(0, i)
  }

  /** Prune the given distribution using beaming, which keeps the part of the distribution
    * that is within the alpha (0-1) constant of the top element.
    * @param scores The distribution.
    * @param alpha Parameter to the beam.
    * @return The pruned distribution.
    */
  def pruneByBeam(scores: List[(String, Double)], alpha: Double): List[(String, Double)] = {
    val sorted = scores.sortWith(_._2 > _._2)
    var i = 0
    val beam = sorted(i)._2 * alpha
    while (sorted(i)._2 > beam)
      i += 1
    sorted.slice(0, i)
  }

  /** Formulate all combinations of ("activity", "actor-type") tuple query terms
    * based on found detections.  This is referred to as the AA pattern.
    *
    * @return Tuples of query terms following the AA pattern.
    */
  def formulateQueriesAA(): Array[(String, String)] = {
    // Retrieve the detections, for each detection, convert to common word form(s).
    // For example, "tv" -> "television", "running" -> "run", "running"
    val actorDetections = new ListBuffer[String]()
    actorDetections.appendAll(detections.getDetections(DetectionTypes.Actors))
    var actors = actorDetections.toSet

    // Get rid of general instances if specific instances exist
    if (actors.contains("person")) {
      if (actors.contains("policeman") || actors.contains("child"))
        actors = actors - "person"
    }
    actorDetections.clear()
    actors.foreach(d => actorDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    actors = actorDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    // Formulate query combinations
    val queries = new ListBuffer[(String, String)]()
    actors.foreach(a => queries.append((activity, a)))

    queries.toArray
  }

  /** Formulate all combinations of ("activity", "actor-type", "location") triplet query terms
    * based on found detections.  This is referred to as the AAL pattern.
    *
    * @return Triplets of query terms following the AAL pattern.
    */
  def formulateQueriesAAL(): Array[(String, String, String)] = {
    // Retrieve the detections, for each detection, convert to common word form(s).
    // For example, "tv" -> "television", "running" -> "run", "running"
    val actorDetections = new ListBuffer[String]()
    actorDetections.appendAll(detections.getDetections(DetectionTypes.Actors))
    var actors = actorDetections.toSet

    // Get rid of general instances if specific instances exist
    if (actors.contains("person")) {
      if (actors.contains("policeman") || actors.contains("child"))
        actors = actors - "person"
    }
    actorDetections.clear()
    actors.foreach(d => actorDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    actors = actorDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    val locationDetections = new ListBuffer[String]()
    detections.getDetections(DetectionTypes.Locations).foreach(d =>
      locationDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    val locations = locationDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    // Formulate query combinations
    val queries = new ListBuffer[(String, String, String)]()
    actors.foreach(a => {
      locations.foreach(l => {
        queries.append((activity, a, l))
      })
    })

    queries.toArray
  }

  /** Formulate all combinations of ("activity", "actor-type", "relationship") triplet query terms
    * based on found detections.  This is referred to as the AAR pattern.
    *
    * @return Triplets of query terms following the AAR pattern.
    */
  def formulateQueriesAAR(): Array[(String, String, String)] = {
    // Retrieve the detections, for each detection, convert to common word form(s).
    // For example, "tv" -> "television", "running" -> "run", "running"
    val actorDetections = new ListBuffer[String]()
    actorDetections.appendAll(detections.getDetections(DetectionTypes.Actors))
    var actors = actorDetections.toSet

    // Get rid of general instances if specific instances exist
    if (actors.contains("person")) {
      if (actors.contains("policeman") || actors.contains("child"))
        actors = actors - "person"
    }
    actorDetections.clear()
    actors.foreach(d => actorDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    actors = actorDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    val relationshipDetections = new ListBuffer[String]()
    detections.getDetections(DetectionTypes.Relationships).foreach(d =>
      relationshipDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    val relationships = relationshipDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    // Formulate query combinations
    val queries = new ListBuffer[(String, String, String)]()
    actors.foreach(a => {
      relationships.foreach(r => {
        queries.append((activity, a, r))
      })
    })

    queries.toArray
  }

  /** Run a terms query using the Document Neighborhood model and return the results as a distribution.
    *
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param targetPOSLabels Set of POS labels used to filter the target tokens
    *                        (default to adjective & verb labels).
    *
    * @return List of normalized (word, score) pairs from the dictionary in alphabetical order.
    */
  def docQuery(searchTerms: List[(String, Option[Set[String]])],
               targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs)): List[(String, Double)] = {
    if (!docModel.isDefined) {
      throw new Exception("ERROR: Uninitialized Document Neighborhood model!")
    }

    // The resulting dictionary
    var frequency: mutable.Map[String, Double] = null

    val cacheFilename = searchTerms.map(_._1.toLowerCase.trim).sorted.mkString("-") + ".txt"
    var file: Option[File] = None
    if (distCacheDirectory.isDefined) {
      file = Option(new File(distCacheDirectory.get + "/doc/" + cacheFilename))
    }

    // Try looking up cached results first
    if (file.isDefined && !overwriteCache && file.get.exists()) {
      frequency = readCacheScores(file.get)
    } else {
      // Reset the model's scores & perform search
      docModel.get.reset()
      docModel.get.processSearchTermsWithPOS(searchTerms, IndexSearcher.DEFAULT_MAX_HITS, targetPOSLabels,
        cacheDir = docModelDataDir)

      frequency = docModel.get.dictionary.freq(0).map(entry => (entry._1(0), entry._2.toDouble))

      // Cache results to file
      if (file.isDefined) {
        writeCacheScores(file.get, frequency)
      }
    }

    // Retrieve the ranked results, normalized to probability
    val results = docModel.get.getRankedResults(DocNeighborhood.Scoring.RelevanceRate, frequency = frequency)
    Stats.normalizeScores(results.sortBy(_._1))
  }

  /** Run a terms query using the Vector Neighborhood model and return the results as a distribution.
    *
    * @param searchTerms List of search terms.
    *
    * @return List of normalized (word, score) pairs from the dictionary in alphabetical order.
    */
  def vecQuery(searchTerms: List[String]): List[(String, Double)] = {
    if (!vecModel.isDefined) {
      throw new Exception("ERROR: Uninitialized Vector Neighborhood model!")
    }

    // The resulting scores
    var scores: mutable.Map[String, Double] = null

    val cacheFilename = searchTerms.map(_.toLowerCase.trim).sorted.mkString("-") + ".txt"
    var file: Option[File] = None
    if (distCacheDirectory.isDefined) {
      file = Option(new File(distCacheDirectory.get + "/vec/" + cacheFilename))
    }

    // Try looking up cached results first
    if (file.isDefined && !overwriteCache && file.get.exists()) {
      scores = readCacheScores(file.get)
    } else {
      // If no cache of results, run new search
      vecModel.get.reset()
      vecModel.get.process(searchTerms, distance = VecNeighborhood.Scoring.ExpCosineBased)

      scores = vecModel.get.scores

      // Cache results to file
      if (file.isDefined) {
        writeCacheScores(file.get, scores)
      }
    }

    // Retrieve the ranked results, normalized to probability
    val results = vecModel.get.getRankedResults(VecNeighborhood.Scoring.ExpCosineBased, scores = scores)
    Stats.normalizeScores(results.sortBy(_._1))
  }

  /** Run a terms query using the Web Neighborhood model and return the results as a distribution.
    *
    * @param searchTerms List of search terms and their associated POS restrictions
    *                    (set to None for no restrictions).
    * @param targetPOSLabels Set of POS labels used to filter the target tokens
    *                        (default to adjective & verb labels).
    *
    * @return List of normalized (word, score) pairs from the dictionary in alphabetical order.
    */
  def webQuery(searchTerms: List[(String, Option[Set[String]])],
               targetPOSLabels: Set[String] = NLPUtils.POS_JJs.union(NLPUtils.POS_VBs)): List[(String, Double)] = {
    if (!webModel.isDefined) {
      throw new Exception("ERROR: Uninitialized Document Neighborhood model!")
    }

    // The resulting scores
    var scores: mutable.Map[String, Double] = null

    val cacheFilename = searchTerms.map(_._1.toLowerCase.trim).mkString("-") + ".txt"
    var file: Option[File] = None
    if (distCacheDirectory.isDefined) {
      file = Option(new File(distCacheDirectory.get + "/web/" + cacheFilename))
    }

    // Try looking up cached results first
    if (file.isDefined && !overwriteCache && file.get.exists()) {
      scores = readCacheScores(file.get)
    } else {
      // If no cache of results, run new search
      webModel.get.reset()
      webModel.get.searchTermsWithPOS(searchTerms, webModelDataDir, targetPOSLabels)

      scores = webModel.get.scores

      // Cache results to file
      if (file.isDefined) {
        writeCacheScores(file.get, scores)
      }
    }

    // Retrieve the ranked results, normalized to probability
    val results = webModel.get.getRankedResults(WebNeighborhood.Scoring.Score, scores = scores)
    Stats.normalizeScores(results.sortBy(_._1))
  }

  /** Implements a back-off + linear-interpolation model where the query results at each level is
    * linearly interpolated into a final distribution.
    *
    * Based on Collins1997.
    *
    * @param queries The different query sets, each containing multiple back-off levels.
    * @param lambdas Parameters of linear interpolation.
    * @param model The type of neighborhood model to use for queries, defaults to DOC.
    * @param eval The evaluation labels for score tuning (for DEBUG purposes, defaults to None).
    *
    * @return Distribution over mental states for the query, as list of (word, score) pairs.
    */
  def backOffLinearInterpolation(queries: Array[List[(String, Option[Set[String]])]],
                                 lambdas: Array[Double], model: ModelTypes = ModelTypes.Document,
                                 eval: Option[Evaluation] = None): List[(String, Double)] = {
    // Generate the underlying query function base on the model type
    var queryFunction: List[(String, Option[Set[String]])] => List[(String, Double)] = null
    model match {
      case ModelTypes.Document => {
        def search(searchTerms: List[(String, Option[Set[String]])]) = docQuery(searchTerms)
        queryFunction = search
      }
      case ModelTypes.Vector => {
        def search(searchTerms: List[(String, Option[Set[String]])]) = vecQuery(searchTerms.map(_._1))
        queryFunction = search
      }
      case ModelTypes.Web => {
        def search(searchTerms: List[(String, Option[Set[String]])]) = webQuery(searchTerms)
        queryFunction = search
      }
      case _ => throw new Exception("Unrecognized neighborhood model.")
    }

    // Run back-off linear interpolation based on the underlying information extraction model.
    BackOffLinearInterpolation.processQueryTuple(queries, lambdas, queryFunction, eval)

  }

  /** Run deleted interpolation using the documents-based model search model.
    *
    * Based on Brants2000.
    *
    * @param queries The different query sets, each containing multiple n-gram levels.
    * @param lambdas Parameters of linear interpolation.
    * @param model The type of neighborhood model to use for queries, defaults to DOC-DEL.
    *
    * @return Distribution over mental states for the query, as list of (word, score) pairs.
    */
  def deletedInterpolation(queries: Array[List[(String, Option[Set[String]])]], lambdas: Array[Double],
                           model: ModelTypes = ModelTypes.Document): List[(String, Double)] = {
    // Run query and average the distribution of each triplet
    val allResults = new mutable.HashMap[String, Double]()
    queries.foreach(query => {
      var result: List[(String, Double)] = null
      model match {
        case ModelTypes.Document => {
          result = delDocModel.get.processQueryTuple(query, lambdas)
        }
        case ModelTypes.Coref => {
          result = delCorefModel.get.processQueryTuple(query, lambdas)
        }
        case ModelTypes.NLP => {
          result = delNLPModel.get.processQueryTuple(query, lambdas)
        }
        case _ => throw new Exception("Unrecognized deleted-interpolation model.")
      }
      result.foreach(kv => {
        allResults.put(kv._1, allResults.getOrElse(kv._1, 0.0) + kv._2)
      })
    })
    val distribution = allResults.toList.map(kv => (kv._1, kv._2 / queries.size))
    assert(math.abs(distribution.map(_._2).sum - 1.0) < Stats.DoubleEpsilon)

    distribution.sortWith(_._2 > _._2)

  }

  /** Process a movie using the baseline model, which outputs all mental states from
    * the dictionary as a uniform distribution.
    *
    * The detections and mental states should already be loaded before running the
    * baseline.
    *
    * @param eval Evaluation object.
    *
    * @return List of normalized (word, score) pairs that is the mental states distribution.
    */
  def baseline(eval: Evaluation): List[(String, Double)] = {
    initBaseline()
    val scores = baselineModel.get.getRankedResults(WebNeighborhood.Scoring.Score)
    clearBaseline()
    Stats.normalizeScores(scores)
  }

  /** Process a movie using the word neighborhood model once the detections and mental states
    * have already been loaded.
    *
    * @param eval Evaluation object.
    * @param vecPrunePct Prune parameter for the vector neighborhood.
    * @param docPrunePct Prune parameter for the doc neighborhood.
    * @param delDocPrunePct Prune parameter for the deleted interpolation model.
    * @param delCorefPrunePct Prune parameter for the deleted interpolation with coref-extended highlights model.
    * @param delNLPPrunePct Prune parameter for the deleted interpolation with NLP model.
    * @param debugMode Prints out some extra scores for calibrating if True.
    *
    * @return List of normalized (word, score) pairs that is the mental states distribution.
    */
  def process(eval: Evaluation, vecPrunePct: Option[Double] = Option(0.10), docPrunePct: Option[Double] = Option(0.7),
              delDocPrunePct: Option[Double] = Option(0.85), delCorefPrunePct: Option[Double] = Option(0.65),
              delNLPPrunePct: Option[Double] = Option(0.85), debugMode: Boolean = false): List[(String, Double)] = {

    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     *
     * TODO: Remove all the hardcoded values at some point!
     */

    /** Formulate queries */
    val queries = formulateQueriesAA()
    val queriesWithPOS = queries.map(q => {
      List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
    })


    /**
     * VEC NEIGHBORHOOD
     */
    var vecPruned: List[(String, Double)] = List[(String, Double)]()
    if (vecPrunePct.isDefined) {
      println("\nVEC NEIGHBORHOOD (Prune = " + vecPrunePct.get + ")\n")
      val vecFile = "/Volumes/MyPassport/data/text/entity-vectors/Gigaword-story/story-vecs.bin"
      //val lambdas = Array(0.2, 0.5)
      val lambdas = Array(0.25)
      initVecNeighborhood(vecFile)
      val vecResults = backOffLinearInterpolation(queriesWithPOS, lambdas, ModelTypes.Vector)
      clearVecNeighborhood()

      if (debugMode) {
        println("\nVec results:")
        println(vecResults.mkString(", "))
        println(vecResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(vecResults, eval)
      }

      vecPruned = Stats.normalizeScores(pruneByProbMass(vecResults, vecPrunePct.get))
    }


    /**
     * DOC NEIGHBORHOOD
     */
    var docPruned: List[(String, Double)] = List[(String, Double)]()
    if (docPrunePct.isDefined) {
      println("\nDOC NEIGHBORHOOD (Prune = " + docPrunePct.get + ")\n")
      val docDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/doc"
      val index = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
      //val lambdas = Array(0.2, 0.5)
      val lambdas = Array(0.25)
      initDocNeighborhood(index, dataDir = Option(docDataDir))
      val docResults = backOffLinearInterpolation(queriesWithPOS, lambdas, ModelTypes.Document)
      clearDocNeighborhood()

      if (debugMode) {
        println("\nDoc results:")
        println(docResults.mkString(", "))
        println(docResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(docResults, eval)
      }

      docPruned = Stats.normalizeScores(pruneByProbMass(docResults, docPrunePct.get))
    }


    /**
     * DELETED DOC NEIGHBORHOOD
     *
     * Lambda parameters for search tuples (trained on the pilot videos).
     *
     *    AA (chase): Array(0.0029585798816568047, 0.378698224852071, 0.6183431952662722)
     *
     *    AA (hug): Array(0.0, 0.3916083916083916, 0.6083916083916084)
     */
    var delDocPruned: List[(String, Double)] = List[(String, Double)]()
    if (delDocPrunePct.isDefined) {
      println("\nDEL NEIGHBORHOOD (Prune = " + delDocPrunePct.get + ")\n")
      val lambdas = Array(0.0029585798816568047, 0.378698224852071, 0.6183431952662722)
      val docIndex = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
      val frequencyDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/frequency/doc"
      val highlightsDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/doc"
      initDelDocNeighborhood(docIndex, frequencyDataDir, highlightsDataDir)
      val delDocResults = deletedInterpolation(queriesWithPOS, lambdas, ModelTypes.Document)
      clearDelDocNeighborhood()

      if (debugMode) {
        println("\nDel Doc results:")
        println(delDocResults.mkString(", "))
        println(delDocResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(delDocResults, eval)
      }

      delDocPruned = Stats.normalizeScores(pruneByProbMass(delDocResults, delDocPrunePct.get))
    }


    /**
     * COREF DELETED NEIGHBORHOOD
     *
     * Lambda parameters for search tuples (trained on the pilot videos).
     *
     *    AA (chase-coref): Array(0.0, 0.08650236657417985, 0.9134976334258201)
     *    AA (chase-win-0): Array(0.0024449877750611247, 0.3643031784841076, 0.6332518337408313)
     *    AA (chase-win-1): Array(0.0, 0.32673267326732675, 0.6732673267326733)
     *    AA (chase-win-2): Array(0.0, 0.2620987062769526, 0.7379012937230475)
     *    AA (chase-win-3): Array(0.0, 0.2484516779490134, 0.7515483220509867)
     *
     *    AA (hug-coref): Array(0.0030309833857207003, 0.05814997754827122, 0.938819039066008)
     */
    var delCorefPruned: List[(String, Double)] = List[(String, Double)]()
    if (delCorefPrunePct.isDefined) {
      println("\nDEL COREF NEIGHBORHOOD (Prune = " + delCorefPrunePct.get + ")\n")
      val lambdas = Array(0.0, 0.08650236657417985, 0.9134976334258201)  /* Reminder: Update! */
      val docIndex = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
      val frequencyDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/frequency/nlp-coref"
      val highlightsDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/doc"
      val nlpFile = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/nlp/coref.txt"
      initDelCorefNeighborhood(docIndex, frequencyDataDir, highlightsDataDir, nlpFile)
      val delCorefResults = deletedInterpolation(queriesWithPOS, lambdas, ModelTypes.Coref)
      clearDelCorefNeighborhood()

      if (debugMode) {
        println("\nDel Coref results:")
        println(delCorefResults.mkString(", "))
        println(delCorefResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(delCorefResults, eval)
      }

      delCorefPruned = Stats.normalizeScores(pruneByProbMass(delCorefResults, delCorefPrunePct.get))
    }


    /**
     * NLP DELETED NEIGHBORHOOD
     *
     * Lambda parameters for search tuples (trained on the pilot videos).
     *
     *    AA (chase-nlp): Array(0.0035087719298245615, 0.1523809523809524, 0.844110275689223)
     *    AA (chase-nlp-swirl): Array(0.00341130604288499,0.1476608187134503,0.8489278752436648)
     *    AA (chase-nlp-subject): Array(0.015567765567765568,0.24267399267399267,0.7417582417582418)
     *    AA (chase-nlp-object): Array(0.017733990147783252,0.1960591133004926,0.7862068965517242)
     *
     *    AAL (chase-nlp): Array(0.001876172607879925, 0.12195121951219512, 0.1651031894934334, 0.7110694183864915)
     *
     *    AA (hug-nlp): Array(0.008615188257817485, 0.10561582641991066, 0.8857689853222719)
     */
    var delNLPPruned: List[(String, Double)] = List[(String, Double)]()
    if (delNLPPrunePct.isDefined) {
      println("\nDEL NLP NEIGHBORHOOD (Prune = " + delNLPPrunePct.get + ")\n")
      val lambdas = Array(0.0035087719298245615, 0.1523809523809524, 0.844110275689223)  /* Reminder: Update! */
      val docIndex = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
      val frequencyDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/frequency/nlp"
      val highlightsDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/highlights/doc"
      initDelNLPNeighborhood(docIndex, frequencyDataDir, highlightsDataDir)
      val delNLPResults = deletedInterpolation(queriesWithPOS, lambdas, ModelTypes.NLP)
      clearDelNLPNeighborhood()

      if (debugMode) {
        println("\nDel NLP results:")
        println(delNLPResults.mkString(", "))
        println(delNLPResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(delNLPResults, eval)
      }

      delNLPPruned = Stats.normalizeScores(pruneByProbMass(delNLPResults, delNLPPrunePct.get))
    }


    /** Combine for final results */
    val resultsMap = new mutable.HashMap[String, Double]()
    val allPairs: ListBuffer[(String, Double)] = new ListBuffer[(String, Double)]()
    var numDistributions = 0.0
    if (vecPrunePct.isDefined && vecPrunePct.get > 0.0 && vecPruned.size > 0) {
      allPairs.appendAll(vecPruned)
      numDistributions += 1.0
    }
    if (docPrunePct.isDefined && docPrunePct.get > 0.0 && docPruned.size > 0) {
      allPairs.appendAll(docPruned)
      numDistributions += 1.0
    }
    if (delDocPrunePct.isDefined && delDocPrunePct.get > 0.0 && delDocPruned.size > 0) {
      allPairs.appendAll(delDocPruned)
      numDistributions += 1.0
    }
    if (delCorefPrunePct.isDefined && delCorefPrunePct.get > 0.0 && delCorefPruned.size > 0) {
      allPairs.appendAll(delCorefPruned)
      numDistributions += 1.0
    }
    if (delNLPPrunePct.isDefined && delNLPPrunePct.get > 0.0 && delNLPPruned.size > 0) {
      allPairs.appendAll(delNLPPruned)
      numDistributions += 1.0
    }
    allPairs.foreach(kv => {
      resultsMap.put(kv._1, resultsMap.getOrElse(kv._1, 0.0) + kv._2)
    })
    val results = resultsMap.toList.map(kv => (kv._1, kv._2 / numDistributions)).sortWith(_._2 > _._2)
    assert(math.abs(results.map(_._2).sum) < Stats.DoubleEpsilon ||
      math.abs(1.0 - results.map(_._2).sum) < Stats.DoubleEpsilon)

    results

  }

}


/** WordNeighborhood singleton object */
object WordNeighborhood {

  /** Different types of models. */
  object ModelTypes extends Enumeration {
    type ModelTypes = Value
    val Document, Vector, Web, Coref, NLP = Value
  }

}


/** Run an information extraction model based on the neighborhood paradigm. */
object RunWordNeighborhood {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Activity and its associated annotation set (26 videos for chase, 45 for hug)
    val activity = "chase"
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + activity + "/xml/"
    val annotationSet = (1 to 26).map(annotationDir + activity + "%02d".format(_) + ".xml")

    // Initialize some file locations needed for the models and evaluation
    val statesDictionary = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val cacheDirectory = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/distributions"
    val mode = ActorMode.All

    // Run baseline model or not
    val runBaseline = false

    println("\n\n===================\nActor Mode: " + mode.toString + "\n===================\n\n")

    // Keep list of scores (for each movie evaluated)
    val F1s = new ListBuffer[(Double, Double, Double)]()
    val CWSAF1s = new ListBuffer[(Double, Double, Double)]()

    // Iterate over each evaluation file
    annotationSet.foreach(annotationFile => {

      println("\n====\nProcessing file " + annotationFile)

      // Create a Word Neighborhood Model
      val wnm = new WordNeighborhood(activity, statesDictionary, distCacheDirectory = Option(cacheDirectory))

      // Evaluation object
      var eval: Evaluation = null

      // Load detections (either for all, only the subject, or only the object).
      mode match {
        case ActorMode.Subject => {
          wnm.loadSubjectDetections(annotationFile)
          eval = new Evaluation(annotationFile, roles = Set(Roles.Subject))
        }

        case ActorMode.Object => {
          wnm.loadObjectDetections(annotationFile)
          eval = new Evaluation(annotationFile, roles = Set(Roles.Object))
        }

        case ActorMode.All => {
          wnm.loadDetections(annotationFile)
          eval = new Evaluation(annotationFile)
        }

        case _ => throw new Exception("Unrecognized mode.")
      }

      if (runBaseline) {

        /** Baseline model */
        println("\n====\nBaseline for file " + annotationFile + "\n")
        val baseline = wnm.baseline(eval)
        F1s.append(eval.F1(baseline.map(_._1).toSet))
        CWSAF1s.append(eval.CWSAF1(baseline))

      } else {

        /** Run model */
        println("\n====\nEvaluation for file " + annotationFile + "\n")

        val distribution = wnm.process(eval, debugMode = false,
          vecPrunePct = Option(.10) /* .10 */ , docPrunePct = None /* .70 */ , delDocPrunePct = None /* .85 */ ,
          delCorefPrunePct = None /* .65 */ , delNLPPrunePct = Option(.55) /* .85 */)

        println("\n====\nFinal results:")
        println(distribution.sortWith(_._2 > _._2).mkString(", "))
        F1s.append(eval.F1(distribution.map(_._1).toSet))
        CWSAF1s.append(eval.CWSAF1(distribution))

      }
    })

    if (F1s.size > 0) {
      println("\n======\nSet F1\n======\n")
      Evaluation.printF1s(F1s.toList)
    }
    if (CWSAF1s.size > 0) {
      println("\n======\nCWSA F1\n======\n")
      Evaluation.printF1s(CWSAF1s.toList)
    }

  }
}

/** Estimate the best prune parameter for an individual model. */
object RunExhaustPrunePercentage {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Activity and its associated annotation set (26 videos for chase, 45 for hug)
    val activity = "chase"
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + activity + "/xml/"
    val annotationSet = (1 to 26).map(annotationDir + activity + "%02d".format(_) + ".xml")

    // Initialize some file locations needed for the models and evaluation
    val statesDictionary = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val cacheDirectory = "/Volumes/MyPassport/data/vlsa/neighborhood/" + activity + "/distributions"
    val mode = ActorMode.All

    // Keep list of scores (for each prune level)
    val F1s = new ListBuffer[(Double, Double, Double)]()
    val CWSAF1s = new ListBuffer[(Double, Double, Double)]()

    // Iterate over each prune level
    for (pct <- (0.0 to 1.0 by 0.05)) {

      println("\n====\nPrune pct: " + pct)

      val f1s = new ListBuffer[(Double, Double, Double)]()
      val cwsaf1s = new ListBuffer[(Double, Double, Double)]()

      // Iterate over each evaluation file
      annotationSet.foreach(annotationFile => {
        // Create a Word Neighborhood Model
        val wnm = new WordNeighborhood(activity, statesDictionary, distCacheDirectory = Option(cacheDirectory))

        // Evaluation object
        var eval: Evaluation = null

        // Load detections (either for all, only the subject, or only the object).
        mode match {
          case ActorMode.Subject => {
            wnm.loadSubjectDetections(annotationFile)
            eval = new Evaluation(annotationFile, roles = Set(Roles.Subject))
          }

          case ActorMode.Object => {
            wnm.loadObjectDetections(annotationFile)
            eval = new Evaluation(annotationFile, roles = Set(Roles.Object))
          }

          case ActorMode.All => {
            wnm.loadDetections(annotationFile)
            eval = new Evaluation(annotationFile)
          }

          case _ => throw new Exception("Unrecognized mode.")
        }

        eval.log = false

        /** Run model */
        val distribution = wnm.process(eval, debugMode = false,
          vecPrunePct = None /* .10 */, docPrunePct = None /* .70 */, delDocPrunePct = None /* .85 */,
          delCorefPrunePct = None /* .70 */, delNLPPrunePct = None /* .85 */)

        f1s.append(eval.F1(distribution.map(_._1).toSet))
        cwsaf1s.append(eval.CWSAF1(distribution))
      })

      F1s.append(Evaluation.aveF1s(f1s.toList))
      println("\n======\nSet F1\n======\n")
      Evaluation.printF1s(f1s.toList)

      CWSAF1s.append(Evaluation.aveF1s(cwsaf1s.toList))
      println("\n======\nCWSA F1\n======\n")
      Evaluation.printF1s(cwsaf1s.toList)
    }

    if (F1s.size > 0) {
      println("\n======\nPrune Ave Set F1\n======\n")
      Evaluation.printF1s(F1s.toList)
    }
    if (CWSAF1s.size > 0) {
      println("\n======\nPrune Ave CWSA F1\n======\n")
      Evaluation.printF1s(CWSAF1s.toList)
    }

  }
}
