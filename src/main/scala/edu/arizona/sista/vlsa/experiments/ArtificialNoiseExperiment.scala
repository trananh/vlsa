package edu.arizona.sista.vlsa.experiments

import edu.arizona.sista.vlsa.main.WordNeighborhood
import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes.DetectionTypes
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode
import edu.arizona.sista.vlsa.utils.NLPUtils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An information extraction experiment that investigates the robustness of the underlying
  * extraction models when given (artificial) noisy detection data.
  *
  * We emulate real vision-based detections by introducing artificial noise to ground-truth
  * detections based on published results in computer vision.  The noisy data is then
  * given to the information extraction models to produce mental state attributes.
  *
  * @constructor An artificial noise detection experiment.
  * @param wnmodel An information-extraction application that uses the word-neighborhood models.
  *
  * @author trananh
  */
class ArtificialNoiseExperiment(val wnmodel: WordNeighborhood) {

  // Track total number of errors introduced
  var truePositives = 0
  var trueNegatives = 0
  var falsePositives = 0
  var falseNegatives = 0

  /** Add false negative errors.
    * Given precision rate p for some detector d from the detections list:
    * - With probability p, keep d in the list, else remove d (false negatives).
    *
    * @param detections Original set of detections.
    * @return New set of detections with random false negatives.
    */
  def introduceFalseNegatives(detections: Set[String]): Set[String] = {
    val results = new ListBuffer[String]()
    detections.foreach(d => {
      if (DetectionsAnnotation.isDetected(d)) {
        results.append(d)
        truePositives += 1
      } else {
        falseNegatives += 1
      }
    })
    results.toSet
  }

  /** Add false positive and false negative errors.
    *
    * Given precision rate p for some detector d from the set of all detections.
    * - If d is in the original list of detections then with probability p, keep d in the list (true positive),
    *     else remove d (false negative).
    * - If d is not in the original list, then with probability p, leave d off the list (true negative),
    *     else add d to the list (false positive).
    *
    * @param detections Original set of detections.
    * @return New set of detections with random false negatives & false positives.
    */
  def introduceFalsePositivesAndFalseNegatives(detections: Set[String], dType: DetectionTypes): Set[String] = {
    val results = new ListBuffer[String]()
    DetectionsAnnotation.DETECTORS_SET.foreach(d => {
      if (DetectionsAnnotation.getType(d).equals(dType)) {
        val isDetected = DetectionsAnnotation.isDetected(d)
        if (detections.contains(d)) {
          // Is in ground-truth, should be accepted
          if (isDetected) {
            results.append(d)
            truePositives += 1
          } else
            falseNegatives += 1
        } else {
          // Is not in ground-truth, should be rejected
          if (isDetected)
            trueNegatives += 1
          else {
            results.append(d)
            falsePositives += 1
          }
        }
      }
    })
    results.toSet
  }

  /** Process a movie using the word neighborhood model once the detections and mental states
    * have already been loaded.
    *
    * @param eval Evaluation object.
    * @param debugMode Prints out some extra scores for calibrating if True.
    *
    * @return List of normalized (word, score) pairs that is the mental states distribution.
    */
  def process(eval: Evaluation, debugMode: Boolean = false): List[(String, Double)] = {

    /* NOTE: All hard-coded file/directory locations below should be changed appropriately before running.
     * There are simply too many of which to expose them as command-line arguments for now.
     *
     * TODO: Remove all the hardcoded values at some point!
     */

    /** Retrieve detections */
    var actors = wnmodel.detections.getDetections(DetectionTypes.Actors).toSet
    var locations = wnmodel.detections.getDetections(DetectionTypes.Locations).toSet


    /** Introduce artificial noise */
    actors = introduceFalsePositivesAndFalseNegatives(actors, DetectionTypes.Actors)
    locations = introduceFalsePositivesAndFalseNegatives(locations, DetectionTypes.Locations)


    /** Add synonyms */
    val actorDetections = new ListBuffer[String]()
    // Get rid of general instances if specific instances exist
    if (actors.contains("person")) {
      if (actors.contains("policeman") || actors.contains("child"))
        actors = actors - "person"
    }
    actors.foreach(d => actorDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    actors = actorDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet

    val locationDetections = new ListBuffer[String]()
    locations.foreach(d => locationDetections.appendAll(DetectionsAnnotation.getCommonWordForms(d)))
    locations = locationDetections.map(e => NLPUtils.lemmatizeTerms(Array(e))(0)).toSet


    /** Formulate queries.
      *
      * Set queries, lambdas, and prune parameters.
      */
    var queriesWithPOS: Array[List[(String, Option[Set[String]])]] = Array[List[(String, Option[Set[String]])]]()
    var vecLambdas: Array[Double] = Array[Double]()
    var delNLPLambdas: Array[Double] = Array[Double]()
    var vecPrunePct: Double = 0.0
    var delNLPPrunePct: Double = 0.0

    if (actors.size > 0 && locations.size > 0) {

      if (debugMode) println("Pattern AAL")

      // Formulate AAL combinations
      val queries = new ListBuffer[(String, String, String)]()
      actors.foreach(a => {
        locations.foreach(l => {
          queries.append((wnmodel.activity, a, l))
        })
      })
      queries.toArray

      // Set queries
      queriesWithPOS = queries.map(q => {
        List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)), (q._3, Option(NLPUtils.POS_NNs)))
      }).toArray

      // Set lambdas
      vecLambdas = Array(0.2, 0.5)
      delNLPLambdas = Array(0.003125, 0.125, 0.1921875, 0.6796875)

      // Set prune pct
      vecPrunePct = 0.15
      delNLPPrunePct = 0.60

    } else if (actors.size > 0) {

      if (debugMode) println("Pattern AA")

      // Formulate AA combinations
      val queries = new ListBuffer[(String, String)]()
      actors.foreach(a => queries.append((wnmodel.activity, a)))
      queries.toArray

      // Set queries
      queriesWithPOS = queries.map(q => {
        List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
      }).toArray

      // Set lambdas
      vecLambdas = Array(0.25)
      delNLPLambdas = Array(0.0035087719298245615, 0.1523809523809524, 0.844110275689223)

      // Set prune pct
      vecPrunePct = 0.10
      delNLPPrunePct = 0.55

    } else if (locations.size > 0) {

      if (debugMode) println("Pattern AL")

      // Formulate AL combinations
      val queries = new ListBuffer[(String, String)]()
      locations.foreach(l => queries.append((wnmodel.activity, l)))
      queries.toArray

      // Set queries
      queriesWithPOS = queries.map(q => {
        List((q._1, Option(NLPUtils.POS_VBs)), (q._2, Option(NLPUtils.POS_NNs)))
      }).toArray

      // Set lambdas
      vecLambdas = Array(0.25)
      delNLPLambdas = Array(0.0012987012987012987, 0.09090909090909091, 0.9077922077922078)

      // Set prune pct
      vecPrunePct = 0.10
      delNLPPrunePct = 0.9

    } else {

      if (debugMode) println("Pattern A")

      // Formulate A combinations
      queriesWithPOS = Array(List((wnmodel.activity, Option(NLPUtils.POS_VBs))))

      // Set lambdas
      delNLPLambdas = Array(0.004310344827586207, 0.9956896551724138)

      // Set prune pct
      vecPrunePct = 0.10
      delNLPPrunePct = 0.35
    }

    /**
     * VEC NEIGHBORHOOD
     */
    var vecPruned: List[(String, Double)] = List[(String, Double)]()
    vecPruned = wnmodel.processVec(eval, queriesWithPOS, vecLambdas, vecPrunePct, debugMode = false)

    /**
     * NLP DELETED NEIGHBORHOOD
     */
    var delNLPPruned: List[(String, Double)] = List[(String, Double)]()
    delNLPPruned = wnmodel.processDelNLP(eval, queriesWithPOS, delNLPLambdas, delNLPPrunePct, debugMode = false)


    /** Combine for final results */
    val resultsMap = new mutable.HashMap[String, Double]()
    val allPairs: ListBuffer[(String, Double)] = new ListBuffer[(String, Double)]()
    var numDistributions = 0.0
    if (vecPruned.size > 0) {
      allPairs.appendAll(vecPruned)
      numDistributions += 1.0
    }
    if (delNLPPruned.size > 0) {
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

object RunArtificialNoiseExperiment {

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

    println("\n\n===================\nActor Mode: " + mode.toString + "\n===================\n\n")

    // Number of trials
    val N = 20

    // Track number of errors added
    var tp, fp, tn, fn = 0

    // Keep list of scores for each trial, up to N trials
    val allTrialsF1s = new ListBuffer[(Double, Double, Double)]()
    val allTrialsCWSAF1s = new ListBuffer[(Double, Double, Double)]()

    // Iterate over each trial
    for (n <- 0 until N) {

      println("- Trial " + n)

      // Keep list of scores (for each movie evaluated)
      val allMoviesF1s = new ListBuffer[(Double, Double, Double)]()
      val allMoviesCWSAF1s = new ListBuffer[(Double, Double, Double)]()

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

        // Evaluate movie with degraded detections
        val experiment = new ArtificialNoiseExperiment(wnm)
        val distribution = experiment.process(eval, debugMode = true)

        tp += experiment.truePositives
        fp += experiment.falsePositives
        tn += experiment.trueNegatives
        fn += experiment.falseNegatives

        // Add movie to trial
        allMoviesF1s.append(eval.F1(distribution.map(_._1).toSet))
        allMoviesCWSAF1s.append(eval.CWSAF1(distribution))

      }) // end movies loop

      // Add trial to experiment
      allTrialsF1s.append(Evaluation.aveF1s(allMoviesF1s.toList))
      allTrialsCWSAF1s.append(Evaluation.aveF1s(allMoviesCWSAF1s.toList))

      // Print progress
      println(allTrialsF1s.last.toString() + allTrialsCWSAF1s.last.toString())

    } // end trials loop


    // Print results
    println("\n======\nSet F1\n======\n")
    Evaluation.printF1s(allTrialsF1s.toList)

    println("\n======\nCWSA F1\n======\n")
    Evaluation.printF1s(allTrialsCWSAF1s.toList)

    // Print stats
    println("\n\n")
    println("true-positives: " + tp + ", " + (tp.toFloat / (N * annotationSet.size)))
    println("false-negatives: " + fn + ", " + (fn.toFloat / (N * annotationSet.size)))
    println("true-negatives: " + tn + ", " + (tn.toFloat / (N * annotationSet.size)))
    println("false-positives: " + fp + ", " + (fp.toFloat / (N * annotationSet.size)))

  }

}
