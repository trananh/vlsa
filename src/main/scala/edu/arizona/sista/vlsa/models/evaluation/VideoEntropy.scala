package edu.arizona.sista.vlsa.models.evaluation

import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.data.VideoAnnotation
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles
import edu.arizona.sista.vlsa.models.data.VideoAnnotation.Roles._
import edu.arizona.sista.vlsa.models.text.ie.neighborhood.NLPNeighborhood.ActorMode
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import edu.arizona.sista.vlsa.utils.NLPUtils
import scala.collection.mutable.ListBuffer

/** Computes the entropy for the distribution of mental state labels in the video.
  *
  * @author trananh
  */
object VideoEntropy {

  /** Load the mental states label from the annotation file and return the probability distribution.
    * Note that the mental states are passed through lemmatization.
    * @param annotationFile The annotation file
    * @param roles Specify which person to extract mental states for (default
    *              to both the subject and the object.
    * @return Mental states probability distribution.
    */
  def loadMentalStates(annotationFile: String, roles: Set[Roles] = Set(Roles.Subject, Roles.Object))
  : List[(String, Double)] = {
    // Get the mental states
    val video = VideoAnnotation.fromXML(annotationFile)
    val terms = new ListBuffer[String]()
    roles.foreach(role => terms.appendAll(video.getMentalStates(role)))

    // Lemmatize the states (as adjectives)
    val lemmas = NLPUtils.lemmatizeTermsAsAdjectives(terms.toArray)

    // Count states frequency
    val statesFrequency = new FrequencyDictionary[String]()
    statesFrequency.addEntries(lemmas.filter(_.length > 0))

    statesFrequency.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortWith(_._2 > _._2)
  }

  /** Main program entry point. */
  def main(args: Array[String]) {

    // Some path locations
    val activity = "chase"
    val annotationDir = "/Volumes/MyPassport/data/annotations/" + activity + "/xml/"
    val mode = ActorMode.Subject

    // Generate annotation set
    val annotationSet = (1 to 26).map(annotationDir + activity + "%02d".format(_) + ".xml")

    // Iterate over each annotation file
    annotationSet.foreach(annotationFile => {

      // The mental state prob distribution
      var mentalStates: List[(String, Double)] = null

      // Load detections (either for all, only the subject, or only the object).
      mode match {
        case ActorMode.Subject => {
          mentalStates = loadMentalStates(annotationFile, roles = Set(Roles.Subject))
        }

        case ActorMode.Object => {
          mentalStates = loadMentalStates(annotationFile, roles = Set(Roles.Object))
        }

        case ActorMode.All => {
          mentalStates = loadMentalStates(annotationFile)
        }

        case _ => throw new Exception("Unrecognized mode.")
      }

      // Compute the entropy for the probability distribution
      val entropy = Stats.entropy(mentalStates.map(_._2))

      // Print it
      println(entropy)

    })

  }

}
