package edu.arizona.sista.vlsa.main

import com.google.gson.Gson
import edu.arizona.sista.vlsa.data.DetectionsAnnotation.DetectionTypes
import edu.arizona.sista.vlsa.data.{DetectionsAnnotation, VideoAnnotation}
import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.main.WordNeighborhood.ModelTypes
import edu.arizona.sista.vlsa.main.WordNeighborhood.ModelTypes.ModelTypes
import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.{WebNeighborhood, VecNeighborhood, DocNeighborhood}
import edu.arizona.sista.vlsa.models.text.ie.{BackOffLinearInterpolation, DeletedInterpolation}
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
  * @param statesDictionary Path to file containing mental state terms.
  * @param distCacheDirectory Directory used for caching intermediate distribution results from
  *                       various queries.
  *
  * @author trananh
  */
class WordNeighborhood(val statesDictionary: String,
                       var distCacheDirectory: Option[String] = None) {

  /** Detection labels discovered from the video. */
  val detections: DetectionsAnnotation = new DetectionsAnnotation()
  var activity: String = "chase"

  /** Information extraction models that uses the word-neighborhood approach. */
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
    while (mass < alpha) {
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
    * @param model The type of neighborhood model to use for queries, defaults to DOC.
    * @param lambdas Parameters of linear interpolation.
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
    *
    * @return Distribution over mental states for the query, as list of (word, score) pairs.
    */
  def deletedInterpolation(queries: Array[List[(String, Option[Set[String]])]],
                           lambdas: Array[Double]): List[(String, Double)] = {

    // Run query and average the distribution of each triplet
    val allResults = new mutable.HashMap[String, Double]()
    queries.foreach(query => {
      val result = delDocModel.get.processQueryTuple(query, lambdas)
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
    * @param debugMode Prints out some extra scores for calibrating if True.
    *
    * @return List of normalized (word, score) pairs that is the mental states distribution.
    */
  def process(eval: Evaluation, vecPrunePct: Option[Double] = Option(0.15), docPrunePct: Option[Double] = Option(0.8),
              delDocPrunePct: Option[Double] = Option(0.65), debugMode: Boolean = false): List[(String, Double)] = {

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
      val docDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/highlights/doc"
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
     */
    var delDocPruned: List[(String, Double)] = List[(String, Double)]()
    if (delDocPrunePct.isDefined) {
      println("\nDEL NEIGHBORHOOD (Prune = " + delDocPrunePct.get + ")\n")
      val lambdas = Array(0.0029411764705882353, 0.36764705882352944, 0.6294117647058823)
      val delDocIndex = "/Volumes/MyPassport/data/text/indexes/Gigaword-stemmed"
      val frequencyDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/frequency/doc"
      val highlightsDataDir = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/highlights/doc"
      initDelDocNeighborhood(delDocIndex, frequencyDataDir, highlightsDataDir)
      val delDocResults = deletedInterpolation(queriesWithPOS, lambdas)
      clearDelDocNeighborhood()

      if (debugMode) {
        println("\nDel Doc results:")
        println(delDocResults.mkString(", "))
        println(delDocResults.sortWith(_._2 > _._2).mkString(", "))
        NeighborhoodExperiment.estimateCWSAF1Params(delDocResults, eval)
      }

      delDocPruned = Stats.normalizeScores(pruneByProbMass(delDocResults, delDocPrunePct.get))
    }


    /** Combine for final results */
    val resultsMap = new mutable.HashMap[String, Double]()
    val allPairs: ListBuffer[(String, Double)] = new ListBuffer[(String, Double)]()
    var numDistributions = 0.0
    if (vecPrunePct.isDefined) {
      allPairs.appendAll(vecPruned)
      numDistributions += 1.0
    }
    if (docPrunePct.isDefined) {
      allPairs.appendAll(docPruned)
      numDistributions += 1.0
    }
    if (delDocPrunePct.isDefined) {
      allPairs.appendAll(delDocPruned)
      numDistributions += 1.0
    }
    allPairs.foreach(kv => {
      resultsMap.put(kv._1, resultsMap.getOrElse(kv._1, 0.0) + kv._2)
    })
    val results = resultsMap.toList.map(kv => (kv._1, kv._2 / numDistributions)).sortWith(_._2 > _._2)
    assert(math.abs(1.0 - results.map(_._2).sum) < Stats.DoubleEpsilon)

    results

  }

}


/** WordNeighborhood singleton object */
object WordNeighborhood {

  /** Different types of models. */
  object ModelTypes extends Enumeration {
    type ModelTypes = Value
    val Document, Vector, Web = Value
  }

}


/** Run an information extraction model based on the neighborhood paradigm. */
object RunWordNeighborhood {

  def main(args: Array[String]) {
    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     * We may consider doing something smarter in the future (e.g., using properties file).
     */

    // Initialize some file locations needed for the models and evaluation
    val statesDictionary = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val cacheDirectory = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/distributions"

    // Generate annotation set
    val annotationDir = "/Volumes/MyPassport/data/annotations/chase/xml/"
    val annotationSet = (1 to 26).map(annotationDir + "chase" + "%02d".format(_) + ".xml")

    // Run baseline model or not
    val runBaseline = false

    // Keep list of scores (for each movie evaluated)
    val F1s = new ListBuffer[(Double, Double, Double)]()
    val SAF1s = new ListBuffer[(Double, Double, Double)]()
    val CWSAF1s = new ListBuffer[(Double, Double, Double)]()

    // Iterate over each evaluation file
    annotationSet.foreach(annotationFile => {

      println("\n====\nProcessing file " + annotationFile)

      // Create a Word Neighborhood Model
      val wnm = new WordNeighborhood(statesDictionary, distCacheDirectory = Option(cacheDirectory))
      wnm.loadDetections(annotationFile)

      // Evaluation object
      val eval = new Evaluation(annotationFile)

      if (runBaseline) {

        /** Baseline model */
        println("\n====\nBaseline for file " + annotationFile + "\n")
        val baseline = wnm.baseline(eval)
        F1s.append(eval.F1(baseline.map(_._1).toSet))
        SAF1s.append(eval.SAF1(baseline.map(_._1)))
        CWSAF1s.append(eval.CWSAF1(baseline))

      } else {

        /** Run model */
        println("\n====\nEvaluation for file " + annotationFile + "\n")
        val distribution = wnm.process(eval, debugMode = false,
          vecPrunePct = Option(.10) /* .10 */, docPrunePct = None /* .70 */, delDocPrunePct = Option(.85) /* .85 */)
        println("\n====\nFinal results:")
        println(distribution.sortWith(_._2 > _._2).mkString(", "))
        F1s.append(eval.F1(distribution.map(_._1).toSet))
        SAF1s.append(eval.SAF1(distribution.map(_._1)))
        CWSAF1s.append(eval.CWSAF1(distribution))

      }
    })

    // Print scores
    def printF1s(f1s: List[(Double, Double, Double)]) = {
      val f = f1s.map(_._1)
      val p = f1s.map(_._2)
      val r = f1s.map(_._3)
      println("\nAvgP, AveR, AveF1\n" + ("=" * 17))
      println((p.sum / p.size) + ", " + (r.sum / r.size) + ", " + (f.sum / f.size))
      println("\nPrecisions\n" + p.mkString("\n"))
      println("\nRecalls\n" + r.mkString("\n"))
      println("\nF1s\n" + f.mkString("\n"))
    }

    if (F1s.size > 0) {
      println("\n======\nSet F1\n======\n")
      printF1s(F1s.toList)
    }
    if (SAF1s.size > 0) {
      println("\n======\nSA F1\n======\n")
      printF1s(SAF1s.toList)
    }
    if (CWSAF1s.size > 0) {
      println("\n======\nCWSA F1\n======\n")
      printF1s(CWSAF1s.toList)
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

    val activity = "chase"

    // Initialize some file locations needed for the models and evaluation
    val statesDictionary = "/Volumes/MyPassport/data/text/dictionaries/mental-states/states-adjectives.txt"
    val annotationDir = "/Volumes/MyPassport/data/annotations/chase/xml/"
    val cacheDirectory = "/Volumes/MyPassport/data/vlsa/neighborhood/chase/distributions"

    // Generate annotation set
    val annotationSet = (1 to 26).map(annotationDir + "chase" + "%02d".format(_) + ".xml")

    // Keep list of scores (for each prune level)
    val F1s = new ListBuffer[(Double, Double, Double)]()
    val CWSAF1s = new ListBuffer[(Double, Double, Double)]()

    // Find the average
    def aveF1s(f1s: ListBuffer[(Double, Double, Double)]): (Double, Double, Double)  = {
      val f = f1s.map(_._1)
      val p = f1s.map(_._2)
      val r = f1s.map(_._3)
      (f.sum / f.size, p.sum / p.size, r.sum / r.size)
    }

    // Iterate over each prune level
    for (pct <- (0.0 to 1.0 by 0.05)) {

      println("\n====\nPrune pct: " + pct)

      val f1s = new ListBuffer[(Double, Double, Double)]()
      val cwsaf1s = new ListBuffer[(Double, Double, Double)]()

      // Iterate over each evaluation file
      annotationSet.foreach(annotationFile => {
        // Create a Word Neighborhood Model
        val wnm = new WordNeighborhood(statesDictionary, distCacheDirectory = Option(cacheDirectory))
        wnm.loadDetections(annotationFile)

        // Evaluation object
        val eval = new Evaluation(annotationFile)
        eval.log = false

        /** Run model */
        val distribution = wnm.process(eval, debugMode = false,
          vecPrunePct = None /* .10 */, docPrunePct = None /* .70 */, delDocPrunePct = None /* .85 */)

        f1s.append(eval.F1(distribution.map(_._1).toSet))
        cwsaf1s.append(eval.CWSAF1(distribution))
      })

      F1s.append(aveF1s(f1s))
      println("\n====\nF1: " + F1s.last.toString())
      CWSAF1s.append(aveF1s(cwsaf1s))
      println("\n====\nCWSA-F1: " + CWSAF1s.last.toString())
    }

    // Print scores
    def printF1s(f1s: ListBuffer[(Double, Double, Double)]) = {
      val f = f1s.map(_._1)
      val p = f1s.map(_._2)
      val r = f1s.map(_._3)
      println("\nPrecisions\n" + p.mkString("\n"))
      println("\nRecalls\n" + r.mkString("\n"))
      println("\nF1s\n" + f.mkString("\n"))
    }

    if (F1s.size > 0) {
      println("\n======\nSet F1\n======\n")
      printF1s(F1s)
    }
    if (CWSAF1s.size > 0) {
      println("\n======\nCWSA F1\n======\n")
      printF1s(CWSAF1s)
    }

  }
}