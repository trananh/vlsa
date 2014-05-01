package edu.arizona.sista.vlsa.models.data

import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes.DetectionTypes
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import edu.arizona.sista.vlsa.utils.NLPUtils

/** A video annotation that stores detection labels, categorized by types.
  *
  * @constructor A video annotation to store detection labels by types.
  *
  * @author trananh
  */
class DetectionsAnnotation() {

  val detections = new mutable.HashMap[DetectionTypes, ListBuffer[String]]()

  /** Add more detection labels to the annotation.
    * @param labels New detection labels.
    */
  def addDetections(labels: String*) = {
    var dType = DetectionTypes.Others
    labels.foreach(l => {
      dType = DetectionsAnnotation.getType(l)
      if (!detections.contains(dType)) {
        detections.put(dType, new ListBuffer[String]())
      }
      detections.get(dType).get.append(l)
    })
  }

  /** Returns all detections of a specific type.
    * @param detectionType Type of detections to get.
    * @return List of detections of a specific type.
    */
  def getDetections(detectionType: DetectionTypes): Array[String] = {
    detections.get(detectionType).get.toArray
  }

  /** Returns all detections.
    * @return List of all detections
    */
  def getAllDetections(): Array[String] = {
    val all = new ListBuffer[String]()
    detections.values.foreach(list => all.appendAll(list))
    all.toArray
  }

}

/** VideoAnnotation singleton object */
object DetectionsAnnotation {

  /** Different types of detections. */
  object DetectionTypes extends Enumeration {
    type DetectionTypes = Value
    val Actions, Actors, Objects, Locations, Relationships, Others = Value
  }

  /** Return the type of the given detection. */
  def getType(detection: String): DetectionTypes = {
    detectionsTypes.getOrElse(detection, DetectionTypes.Others)
  }

  /** Map the given detection to its commonly used word forms. */
  def getCommonWordForms(detection: String): Array[String] = {

    /* If action/verb, then return base-form */
    if (getType(detection).equals(DetectionTypes.Actions)) {
      val lemma = NLPUtils.lemmatizeTerms(Array(detection))(0)
      if (lemma.equals(detection))
        return Array(detection)
      return Array(detection, lemma)
    }

    detection match {

      /** Translate common abbreviations and slangs */
      case "bike"       => return Array("bicycle")
      case "motor-bike" => return Array("motorcycle")
      case "tv"         => return Array("television")

      /** Use common terms for parent-child relationship */
      case "parent-child" => Array("parent", "father", "mother", "dad", "mom", "child", "son", "daughter")

      /** Introduce common synonyms using the most frequent word-sense synset from WordNet 3.1.
        * Note: we only use synonyms that are single words (e.g. no "police officer").
        */
      case "child"      => return Array("child", "kid", "youngster", "minor", "shaver", "nipper", "tiddler", "tike",
                                        "tyke", "fry", "nestling")
      case "policeman"  => return Array("policeman", "officer")
      case "person"     => return Array("person", "individual", "someone", "somebody", "mortal", "soul")

      case _            => return Array(detection)
    }
  }

  /** Map of detections to the category type */
  private val detectionsTypes = HashMap[String, DetectionTypes] (
    /* Actions */
    "carrying"      -> DetectionTypes.Actions,
    "chase"         -> DetectionTypes.Actions,
    "fall"          -> DetectionTypes.Actions,
    "jogging"       -> DetectionTypes.Actions,
    "jumping"       -> DetectionTypes.Actions,
    "kicking"       -> DetectionTypes.Actions,
    "lifting"       -> DetectionTypes.Actions,
    "pick-up"       -> DetectionTypes.Actions,
    "pulling"       -> DetectionTypes.Actions,
    "pushing"       -> DetectionTypes.Actions,
    "put-down"      -> DetectionTypes.Actions,
    "raising"       -> DetectionTypes.Actions,
    "running"       -> DetectionTypes.Actions,
    "walking"       -> DetectionTypes.Actions,
    "waving"        -> DetectionTypes.Actions,

    /* Actor types */
    "child"         -> DetectionTypes.Actors,
    "object"        -> DetectionTypes.Actors,
    "person"        -> DetectionTypes.Actors,
    "policeman"     -> DetectionTypes.Actors,

    /* Object types */
    "bag"           -> DetectionTypes.Objects,
    "ball"          -> DetectionTypes.Objects,
    "bat"           -> DetectionTypes.Objects,
    "bike"          -> DetectionTypes.Objects,
    "boat"          -> DetectionTypes.Objects,
    "bus"           -> DetectionTypes.Objects,
    "car"           -> DetectionTypes.Objects,
    "chair"         -> DetectionTypes.Objects,
    "gun"           -> DetectionTypes.Objects,
    "motor-bike"    -> DetectionTypes.Objects,
    "table"         -> DetectionTypes.Objects,
    "tv"            -> DetectionTypes.Objects,
    "train"         -> DetectionTypes.Objects,
    "tree"          -> DetectionTypes.Objects,
    "truck"         -> DetectionTypes.Objects,

    /* Location types */
    "alley"         -> DetectionTypes.Locations,
    "arena"         -> DetectionTypes.Locations,
    "backyard"      -> DetectionTypes.Locations,
    "beach"         -> DetectionTypes.Locations,
    "country"       -> DetectionTypes.Locations,
    "field"         -> DetectionTypes.Locations,
    "forest"        -> DetectionTypes.Locations,
    "highway"       -> DetectionTypes.Locations,
    "home"          -> DetectionTypes.Locations,
    "indoor"        -> DetectionTypes.Locations,
    "industrial"    -> DetectionTypes.Locations,
    "office"        -> DetectionTypes.Locations,
    "outdoor"       -> DetectionTypes.Locations,
    "park"          -> DetectionTypes.Locations,
    "parking-lot"   -> DetectionTypes.Locations,
    "playground"    -> DetectionTypes.Locations,
    "store"         -> DetectionTypes.Locations,
    "street"        -> DetectionTypes.Locations,
    "suburb"        -> DetectionTypes.Locations,

    /* Relationship types */
    "strangers"     -> DetectionTypes.Relationships,
    "acquaintances" -> DetectionTypes.Relationships,
    "friends"       -> DetectionTypes.Relationships,
    "lovers"        -> DetectionTypes.Relationships,
    "siblings"      -> DetectionTypes.Relationships,
    "parent-child"  -> DetectionTypes.Relationships,

    /* Other detections */
    "chasee-multi"  -> DetectionTypes.Others,
    "chaser-multi"  -> DetectionTypes.Others,
    "hug-multi"     -> DetectionTypes.Others

  )

  /** Map of detections to their empirical accuracies */
  private val detectionsAccuracies = HashMap[String, Double] (
    /* Actions */
    "carrying"      -> 0.0,     /** UNKNOWN */
    "chase"         -> 0.0,     /** UNKNOWN */
    "fall"          -> 0.0,     /** UNKNOWN */
    "jogging"       -> 1.0,     /** Sadanand2012 - KTH Actions */
    "jumping"       -> 0.8,     /** Kerr2011 - ww3d */
    "kicking"       -> 1.0,     /** Sadanand2012 - UCF Sports */
    "lifting"       -> 0.83,    /** Sadanand2012 - UCF Sports */
    "pick-up"       -> 0.0,     /** UNKNOWN */
    "pulling"       -> 0.0,     /** UNKNOWN */
    "pushing"       -> 0.8,     /** Kerr2011 - ww3d */
    "put-down"      -> 0.0,     /** UNKNOWN */
    "raising"       -> 0.0,     /** UNKNOWN */
    "running"       -> 0.91,    /** Sadanand2012 - UCF Sports */
    "walking"       -> 0.86,    /** Sadanand2012 - UCF Sports */
    "waving"        -> 1.0,     /** Sadanand2012 - KTH Actions */

    /* Actor types */
    "child"         -> 0.0,     /** UNKNOWN */
    "object"        -> 0.0,     /** UNKNOWN */
    "person"        -> 0.0,     /** UNKNOWN */
    "policeman"     -> 0.0,     /** UNKNOWN */

    /* Object types */
    "bag"           -> 0.0,     /** UNKNOWN */
    "ball"          -> 0.0,     /** UNKNOWN */
    "bat"           -> 0.0,     /** UNKNOWN */
    "bike"          -> 0.0,     /** UNKNOWN */
    "boat"          -> 0.0,     /** UNKNOWN */
    "bus"           -> 0.0,     /** UNKNOWN */
    "car"           -> 0.0,     /** UNKNOWN */
    "chair"         -> 0.0,     /** UNKNOWN */
    "gun"           -> 0.0,     /** UNKNOWN */
    "motor-bike"    -> 0.0,     /** UNKNOWN */
    "table"         -> 0.0,     /** UNKNOWN */
    "tv"            -> 0.0,     /** UNKNOWN */
    "train"         -> 0.0,     /** UNKNOWN */
    "tree"          -> 0.0,     /** UNKNOWN */
    "truck"         -> 0.0,     /** UNKNOWN */

    /* Location types */
    "alley"         -> 0.0,     /** UNKNOWN */
    "arena"         -> 0.0,     /** UNKNOWN */
    "backyard"      -> 0.0,     /** UNKNOWN */
    "beach"         -> 0.0,     /** UNKNOWN */
    "country"       -> 0.0,     /** UNKNOWN */
    "field"         -> 0.0,     /** UNKNOWN */
    "forest"        -> 0.0,     /** UNKNOWN */
    "highway"       -> 0.0,     /** UNKNOWN */
    "home"          -> 0.0,     /** UNKNOWN */
    "indoor"        -> 0.0,     /** UNKNOWN */
    "industrial"    -> 0.0,     /** UNKNOWN */
    "office"        -> 0.0,     /** UNKNOWN */
    "outdoor"       -> 0.0,     /** UNKNOWN */
    "park"          -> 0.0,     /** UNKNOWN */
    "parking-lot"   -> 0.0,     /** UNKNOWN */
    "playground"    -> 0.0,     /** UNKNOWN */
    "store"         -> 0.0,     /** UNKNOWN */
    "street"        -> 0.0,     /** UNKNOWN */
    "suburb"        -> 0.0,     /** UNKNOWN */

    /* Relationship types */
    "strangers"     -> 0.0,     /** UNKNOWN */
    "acquaintances" -> 0.0,     /** UNKNOWN */
    "friends"       -> 0.0,     /** UNKNOWN */
    "lovers"        -> 0.0,     /** UNKNOWN */
    "siblings"      -> 0.0,     /** UNKNOWN */
    "parent-child"  -> 0.0,     /** UNKNOWN */

    /* Other detections */
    "chasee-multi"  -> 0.0,     /** UNKNOWN */
    "chaser-multi"  -> 0.0,     /** UNKNOWN */
    "hug-multi"     -> 0.0      /** UNKNOWN */
  )

}