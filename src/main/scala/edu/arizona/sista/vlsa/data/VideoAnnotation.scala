package edu.arizona.sista.vlsa.data

import edu.arizona.sista.vlsa.data.VideoAnnotation.Roles.Roles
import edu.arizona.sista.vlsa.data.VideoAnnotation.{Roles, AttributeTypes}
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.xml.XML

/** A video annotation that stores mental state and detection labels pertaining to a
  * specific video.
  *
  * @constructor A video annotation to store labels pertaining to the video.
  * @param videoID The video ID.
  * @param activity The main activity of the video.
  *
  * @author trananh
  */
class VideoAnnotation(var videoID: String, var activity: String) {

  /** Dictionaries to store the labels, keyed by grouping types */
  val mentalStates = new HashMap[Roles, ListBuffer[String]]()
  val detections = new HashMap[Roles, DetectionsAnnotation]()

  /** Add more mental state labels to the annotation.
    * @param role The role associated with the new labels.
    * @param labels New mental state labels.
    */
  def addMentalStates(role: Roles, labels: String*) = {
    if (!mentalStates.contains(role)) {
      mentalStates.put(role, new ListBuffer[String]())
    }
    mentalStates.get(role).get.appendAll(labels)
  }
  
  /** Add more detection labels to the annotation.
    * @param role The role associated with the new labels.
    * @param labels New detection labels.
    */
  def addDetections(role: Roles, labels: String*) = {
    if (!detections.contains(role)) {
      detections.put(role, new DetectionsAnnotation())
    }
    detections.get(role).get.addDetections(labels: _*)
  }

  /** Return the mental state labels for the given role.
    * @param role The role.
    * @return Mental state labels associated with the role.
    */
  def getMentalStates(role: Roles): Array[String] = {
    if (mentalStates.contains(role))
      return mentalStates.get(role).get.toArray
    Array[String]()
  }

  /** Return the detection labels for the given role.
    * @param role The role.
    * @return Detection labels associated with the role.
    */
  def getDetections(role: Roles): Array[String] = {
    if (detections.contains(role))
      return detections.get(role).get.getAllDetections()
    Array[String]()
  }

  /** Convert the annotation to XMl format.
    * @return The annotation in XML format.
    */
  def toXML(): String =  {
    val sb = new StringBuilder()

    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
    sb.append("<annotation id=\"" + videoID + "\" activity=\"" + activity + "\" source=\"MTurk\">\n")
    for ((role, i) <- Roles.values.zipWithIndex) {
      sb.append((" " * 4) + "<object id=\"" + i + "\" role=\"" + role.toString + "\">\n")
      if (mentalStates.contains(role)) {
        sb.append((" " * 8) + "<attribute type=\"" + AttributeTypes.Mental.toString + "\">")
        sb.append(mentalStates.get(role).get.sorted.mkString(", ") + "</attribute>\n")
      } else
        sb.append((" " * 8) + "<attribute type=\"" + AttributeTypes.Mental.toString + "\" />\n")
      if (detections.contains(role)) {
        sb.append((" " * 8) + "<attribute type=\"" + AttributeTypes.Detection.toString + "\">")
        sb.append(detections.get(role).get.getAllDetections().sorted.mkString(", ") + "</attribute>\n")
      } else
        sb.append((" " * 8) + "<attribute type=\"" + AttributeTypes.Detection.toString + "\" />\n")
      sb.append((" " * 4) + "</object>\n")
    }
    sb.append("</annotation>")

    sb.toString()
  }

  /** Override toString to show XML formatted string */
  override def toString = toXML()

}

/** VideoAnnotation singleton object */
object VideoAnnotation {

  /** Different roles of the objects. */
  object Roles extends Enumeration {
    type Roles = Value
    val Subject, Object, Settings, Others = Value
  }

  /** Different types of attributes. */
  object AttributeTypes extends Enumeration {
    type AttributeTypes = Value
    val Mental, Detection = Value
  }

  /** Load the video annotation from XML.
    * @param annotationFile Path to the annotation file.
    */
  def fromXML(annotationFile: String): VideoAnnotation = {
    val annotation = XML.loadFile(annotationFile)
    val video = new VideoAnnotation((annotation \ "@id").text, (annotation \ "@activity").text)
    (annotation \ "object").foreach(obj => {
      val role = Roles.withName((obj \ "@role").text)
      (obj \ "attribute").foreach(lbl => {
        val labels = lbl.text.trim.split(",").map(_.trim.toLowerCase).filter(_.length > 0)
        if (labels.size > 0) {
          AttributeTypes.withName((lbl \ "@type").text) match {
            case AttributeTypes.Mental => video.addMentalStates(role,  labels :_*)
            case AttributeTypes.Detection => video.addDetections(role, labels :_*)
          }
        }
      })
    })
    video
  }

}
