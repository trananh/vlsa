package edu.arizona.sista.vlsa.models.data

import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes
import edu.arizona.sista.vlsa.models.data.DetectionsAnnotation.DetectionTypes.DetectionTypes
import edu.arizona.sista.vlsa.utils.NLPUtils

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

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

  /** Random number generator */
  private val randomizer = new Random()

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
      case "bike"         => return Array("bicycle")
      case "motor-bike"   => return Array("motorcycle")
      case "tv"           => return Array("television")

      /** Use common terms for parent-child relationship */
      case "parent-child" => Array("parent", "father", "mother", "dad", "mom", "child", "son", "daughter")

      /** Introduce common synonyms using the most frequent word-sense synset from WordNet 3.1.
        * Note: we only use synonyms that are single words (e.g. no "police officer").
        */
      case "child"        => return Array("child", "kid", "youngster", "minor", "shaver", "nipper", "tiddler",
                                          "tike", "tyke", "fry", "nestling")
      case "policeman"    => return Array("policeman", "officer")
      case "person"       => return Array("person", "individual", "someone", "somebody", "mortal", "soul")

      case "alley"        => return Array("alley", "alleyway", "backstreet")
      case "arena"        => return Array("arena", "sphere", "domain", "area", "orbit", "field")
      case "backyard"     => return Array("backyard")
      case "beach"        => return Array("beach")
      case "country"      => return Array("country", "state", "nation", "land", "commonwealth")
      case "field"        => return Array("field")
      case "forest"       => return Array("forest", "wood", "woods")
      case "highway"      => return Array("highway")
      case "home"         => return Array("home", "place")
      case "indoor"       => return Array("indoor")
      case "industrial"   => return Array("industrial")
      case "office"       => return Array("office")
      case "outdoor"      => return Array("outdoor", "outside")
      case "park"         => return Array("park", "parkland")
      case "parking-lot"  => return Array("parking-lot", "park")
      case "playground"   => return Array("playground")
      case "store"        => return Array("store", "shop")
      case "street"       => return Array("street")
      case "suburb"       => return Array("suburb", "suburbia")

      case _              => return Array(detection)
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

  /** Set of all possible detectors. */
  val DETECTORS_SET: Set[String] = detectionsTypes.keySet



  /** Return the accuracy percentage of the given detection. */
  def getAccuracyPercentage(detection: String): Double = {
    detectionsAccuracies.getOrElse(detection, 0.0)
  }

  /** Return true if the detection triggered, based on accuracy percentage.
    *
    * In other words, add artificial noise to a detection based on known accuracy.
    */
  def isDetected(detection: String): Boolean = {
    val i = randomizer.nextInt(100).toDouble
    (i < getAccuracyPercentage(detection) * 100)
  }

  /** Map of detections to their empirical accuracies */
  private val detectionsAccuracies = HashMap[String, Double] (
    /* Actions */
    "carrying"      -> 0.0,     /** UNKNOWN */
    "chase"         -> 0.99690, /** UA Mind's Eye Y1 evaluation */
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
    "child"         -> 0.461,   /** VOC2012 "person" detection precision (UOC_OXFORD_DPM_MKL) */
    "object"        -> 0.4113,  /** VOC2012 average (non-person) detection precision (UVA_HYBRID_CODING_APE) */
    "person"        -> 0.461,   /** VOC2012 "person" detection precision (UOC_OXFORD_DPM_MKL) */
    "policeman"     -> 0.4086,  /** VOC2012 average detection precision (UVA_HYBRID_CODING_APE) */

    /* Object types */
    "bag"           -> 0.0,     /** UNKNOWN */
    "ball"          -> 0.0,     /** UNKNOWN */
    "bat"           -> 0.0,     /** UNKNOWN */
    "bike"          -> 0.545,   /** VOC2012 "bicycle" detection precision (UOC_OXFORD_DPM_MKL) */
    "boat"          -> 0.248,   /** VOC2012 "boat" detection precision (UVA_HYBRID_CODING_APE) */
    "bus"           -> 0.571,   /** VOC2012 "bus" detection precision (UVA_HYBRID_CODING_APE) */
    "car"           -> 0.493,   /** VOC2012 "car" detection precision (UOC_OXFORD_DPM_MKL) */
    "chair"         -> 0.195,   /** VOC2012 "chair" detection precision (MISSOURI_HOGLBP_MDPM_CONTEXT) */
    "gun"           -> 0.0,     /** UNKNOWN */
    "motor-bike"    -> 0.594,   /** VOC2012 "motor-bike" detection precision (UVA_HYBRID_CODING_APE) */
    "table"         -> 0.0,     /** UNKNOWN */
    "tv"            -> 0.495,   /** VOC2012 "tv/monitor" detection precision (UVA_HYBRID_CODING_APE) */
    "train"         -> 0.511,   /** VOC2012 "train" detection precision (UVA_HYBRID_CODING_APE) */
    "tree"          -> 0.0,     /** UNKNOWN */
    "truck"         -> 0.0,     /** UNKNOWN */

    /* Location types */
    "alley"         -> 0.231,   /** "alley" precision (Xiao2010) */
    "arena"         -> 0.345,   /** average precision (Xiao2010) */
    "backyard"      -> 0.345,   /** average precision (Xiao2010) */
    "beach"         -> 0.184,   /** "beach" precision (Xiao2010) */
    "country"       -> 0.705,   /** "open-country" accuracy (Lazebnik2006) */
    "field"         -> 0.345,   /** average precision (Xiao2010) */
    "forest"        -> 0.947,   /** "forest" accuracy (Lazebnik2006) */
    "highway"       -> 0.866,   /** "highway" accuracy (Lazebnik2006) */
    "home"          -> 0.629,   /** average indoor performance Gist (Quattoni2009) */
    "indoor"        -> 0.629,   /** average indoor performance Gist (Quattoni2009) */
    "industrial"    -> 0.654,   /** "industrial" accuracy (Lazebnik2006) */
    "office"        -> 0.927,   /** "office" accuracy (Lazebnik2006) */
    "outdoor"       -> 0.345,   /** average precision (Xiao2010) */
    "park"          -> 0.48,    /** "park" precision (Xiao2010) */
    "parking-lot"   -> 0.345,   /** average precision (Xiao2010) */
    "playground"    -> 0.467,   /** "playground" precision (Xiao2010) */
    "store"         -> 0.762,   /** "store" accuracy (Lazebnik2006) */
    "street"        -> 0.805,   /** "street" accuracy (Lazebnik2006) */
    "suburb"        -> 0.994,   /** "suburb" accuracy (Lazebnik2006) */

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