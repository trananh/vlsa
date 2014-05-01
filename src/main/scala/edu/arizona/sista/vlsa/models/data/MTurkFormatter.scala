package edu.arizona.sista.vlsa.models.data

import VideoAnnotation.Roles
import com.martiansoftware.jsap.{JSAPResult, FlaggedOption, Switch, JSAP}
import edu.arizona.sista.vlsa.struct.FrequencyDictionary
import java.io.{FileWriter, BufferedWriter, File}
import scala.collection.mutable.HashMap

/** A formatter to format MTurk CSV annotation to ground-truth XML files.
  *
  * @constructor A video annotation formatter.
  * @param indexFile A video index, mapping video URLs to their designated IDs/names.
  *
  * @author trananh
  */
class MTurkFormatter(indexFile: Option[String] = None) {

  /** An index mapping video URLs to their designated IDs/names. */
  val videoIndex = new HashMap[String, String]()
  if (indexFile.isDefined) loadVideoIndex(indexFile.get)

  /** Set of annotated videos mapped by their IDs. */
  val videos = new HashMap[String, VideoAnnotation]()

  /** Strip off white spaces and double quotes at the beginning or end of the string. */
  private def strip(str: String): String = str.trim().stripPrefix("\"").stripSuffix("\"")

  /** Load the video index from an index CSV file.
    * The CSV file should contain a header line with the following columns:
    *   "video" - the video URL
    *   "id" - the ID/name of the video
    * @param indexFile Path to the video index file.
    */
  def loadVideoIndex(indexFile: String) {
    // Open file and grab the necessary column index using the headers
    val linesIterator = scala.io.Source.fromFile(new File(indexFile)).getLines()
    var row = linesIterator.next().split(",", -1).map(strip(_).trim.toLowerCase)
    val videoUrlIdx = row.indexOf("video")
    val idIdx = row.indexOf("id")

    // Read index csv file
    linesIterator.foreach(line => {
      row = line.split(",", -1).map(strip(_).trim)
      videoIndex.put(row(videoUrlIdx), row(idIdx))
    })
  }

  /** Load the mental state MTurk annotation from a CSV file.
    * The CSV file should contain a header line and the following columns:
    *   videoHeader - the video ID
    *   subHeader - mental states of the subject actor, delimited by semicolons
    *   objHeader - mental states of the object actor, delimited by semicolons
    * @param turkMentalStatesFile Path to the mental states annotation file.
    * @param activity The activity (defaults to chase).
    * @param subHeader The column header for the subject's answers (defaults to answer.chaser).
    * @param objHeader The column header for the object's answers (defaults to answer.chasee).
    * @param videoHeader The column header for the video id (defaults to input.embedded_video_url).
    */
  def loadMentalStatesGold(turkMentalStatesFile: String,
                           activity: String = "chase",
                           subHeader: String = "answer.chaser",
                           objHeader: String = "answer.chasee",
                           videoHeader: String = "input.embedded_video_url") {
    // Open data file
    val linesIterator = scala.io.Source.fromFile(new File(turkMentalStatesFile)).getLines()
    var row = linesIterator.next().split(",", -1).map(strip(_).trim.toLowerCase)
    val videoUrlIdx = row.indexOf(videoHeader)
    val subIdx = row.indexOf(subHeader)
    val objIdx = row.indexOf(objHeader)

    // Method for splitting mental state labels
    def split(s: String) = s.toLowerCase.split(";").map(_.trim).filter(_.length > 0)

    // Read mental states
    var video: VideoAnnotation = null
    linesIterator.foreach(line => {
      row = line.split(",", -1).map(strip(_).trim)
      val videoID = videoIndex.get(row(videoUrlIdx)).get
      if (!videos.contains(videoID)) {
        video = new VideoAnnotation(videoID, activity)
        videos.put(videoID, video)
      } else
        video = videos.get(videoID).get
      video.addMentalStates(Roles.Subject, split(row(subIdx)) :_*)
      video.addMentalStates(Roles.Object, split(row(objIdx)) :_*)
    })

    // Let's also count all the mental states adjectives used
    val freq = new FrequencyDictionary[String]()
    videos.values.foreach(m => {
      m.mentalStates.values.foreach(l => {
        freq.addEntries(l)
      })
    })
    val allAdjectives = freq.toSortedList().map(e => (e._1(0), e._2))
    println("Loaded " + allAdjectives.size + " different mental states adjectives.")
    println(allAdjectives.mkString(", "))
  }

  /** Load the detection MTurk annotation from a CSV file.
    * The CSV file should contain a header line and the following columns:
    *   "input.embedded_video_url" - the video URL
    *   "answer.chasers" - detections relevant to the chaser(s), delimited by pipes '|'
    *   "answer.chasees" - detections relevant to the chasee(s), delimited by pipes '|'
    *   "answer.objects" - detections relevant to other objects, delimited by pipes '|'
    *   "answer.settings" - detections relevant to the settings, delimited by pipes '|'
    * @param turkDetectionsFile Path to the detections annotation file.
    */
  def loadChaseDetectionsGold(turkDetectionsFile: String) = {
    // Open data file
    val activity = "chase"
    val linesIterator = scala.io.Source.fromFile(new File(turkDetectionsFile)).getLines()
    var row = linesIterator.next().split(",", -1).map(strip(_).trim.toLowerCase)
    val videoUrlIdx = row.indexOf("input.embedded_video_url")
    val chaserIdx = row.indexOf("answer.chasers")
    val chaseeIdx = row.indexOf("answer.chasees")
    val otherIdx = row.indexOf("answer.objects")
    val settingIdx = row.indexOf("answer.settings")

    // Method for splitting detection labels
    def split(s: String) = s.toLowerCase.split("\\|").map(_.trim).filter(_.length > 0)

    // Read detections
    var video: VideoAnnotation = null
    linesIterator.foreach(line => {
      row = line.split(",", -1).map(strip(_).trim)
      val videoID = videoIndex.get(row(videoUrlIdx)).get
      if (!videos.contains(videoID)) {
        video = new VideoAnnotation(videoID, activity)
        videos.put(videoID, video)
      } else
        video = videos.get(videoID).get
      video.addDetections(Roles.Subject, split(row(chaserIdx)) :_*)
      video.addDetections(Roles.Object, split(row(chaseeIdx)) :_*)
      video.addDetections(Roles.Settings, split(row(settingIdx)) :_*)
      video.addDetections(Roles.Others, split(row(otherIdx)) :_*)
    })

    // Let's also count all the detections used
    val freq = new FrequencyDictionary[String]()
    videos.values.foreach(m => {
      m.detections.values.foreach(d => {
        freq.addEntries(d.getAllDetections())
      })
    })
    val allDetections = freq.toSortedList().map(e => (e._1(0), e._2))
    println("Found " + allDetections.size + " different detections.")
    println(allDetections.mkString(", "))
  }

  /** Load the detection MTurk annotation from a CSV file.
    * The CSV file should contain a header line and the following columns:
    *   "input.embedded_video_url" - the video URL
    *   "answer.hug" - detections relevant to the hug, delimited by pipes '|'
    *   "answer.objects" - detections relevant to other objects, delimited by pipes '|'
    *   "answer.settings" - detections relevant to the settings, delimited by pipes '|'
    * @param turkDetectionsFile Path to the detections annotation file.
    */
  def loadHugDetectionsGold(turkDetectionsFile: String) = {
    // Open data file
    val activity = "hug"
    val linesIterator = scala.io.Source.fromFile(new File(turkDetectionsFile)).getLines()
    var row = linesIterator.next().split(",", -1).map(strip(_).trim.toLowerCase)
    val videoUrlIdx = row.indexOf("input.embedded_video_url")
    val hugIdx = row.indexOf("answer.hug")
    val otherIdx = row.indexOf("answer.objects")
    val settingIdx = row.indexOf("answer.settings")

    // Method for splitting detection labels
    def split(s: String) = s.toLowerCase.split("\\|").map(_.trim).filter(_.length > 0)

    // Read detections
    var video: VideoAnnotation = null
    linesIterator.foreach(line => {
      row = line.split(",", -1).map(strip(_).trim)
      val videoID = videoIndex.get(row(videoUrlIdx)).get
      if (!videos.contains(videoID)) {
        video = new VideoAnnotation(videoID, activity)
        videos.put(videoID, video)
      } else
        video = videos.get(videoID).get
      video.addDetections(Roles.Others, split(row(hugIdx)) :_*)
      video.addDetections(Roles.Others, split(row(otherIdx)) :_*)
      video.addDetections(Roles.Settings, split(row(settingIdx)) :_*)
    })

    // Let's also count all the detections used
    val freq = new FrequencyDictionary[String]()
    videos.values.foreach(m => {
      m.detections.values.foreach(d => {
        freq.addEntries(d.getAllDetections())
      })
    })
    val allDetections = freq.toSortedList().map(e => (e._1(0), e._2))
    println("Found " + allDetections.size + " different detections.")
    println(allDetections.mkString(", "))
  }

  /** Write ground-truth XML files to output directory.
    * @param outDir Path to the output directory.
    */
  def writeGroundTruthXMLs(outDir: String) {
    videos.keySet.foreach(videoID => {
      val file = new File(outDir + "/" + videoID + ".xml")
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write(videos.get(videoID).get.toXML())
      writer.close()
    })
  }

}

/** Format annotation from MTurk into XML files */
object MTurkFormatter {

  def main(args: Array[String]) {

    // Use JSAP to get command line args
    val jsap: JSAP = new JSAP()
    jsap.registerParameter(new Switch("help")
      .setShortFlag('h').setLongFlag("help"))
    jsap.registerParameter(new FlaggedOption("activity")
      .setDefault("").setRequired(true).setLongFlag("activity"))
    jsap.registerParameter(new FlaggedOption("index")
      .setDefault("").setRequired(true).setLongFlag("index"))
    jsap.registerParameter(new FlaggedOption("mental-states-csv")
      .setDefault("").setRequired(true).setLongFlag("mental-states-csv"))
    jsap.registerParameter(new FlaggedOption("detections-csv")
      .setDefault("").setRequired(true).setLongFlag("detections-csv"))
    jsap.registerParameter(new FlaggedOption("out-dir")
      .setDefault("").setRequired(true).setLongFlag("out-dir"))
    val config: JSAPResult = jsap.parse(args)
    if (config.getBoolean("help")
      || "".equals(config.getString("activity"))
      || "".equals(config.getString("index"))
      || "".equals(config.getString("mental-states-csv"))
      || "".equals(config.getString("detections-csv"))
      || "".equals(config.getString("out-dir"))) {
      print("MTurkFormatter ")
      println(jsap.getUsage)
      println("\nExample: ")
      println("MTurkFormatter --activity chase --index index.csv --mental-states-csv results-format.csv " +
        "--detections-csv results-format.csv --out-dir /path/to/out")
      return
    }

    // Get arguments
    val activity = config.getString("activity").trim().toLowerCase
    val indexFile = config.getString("index").trim()
    val mentalGoldFile = config.getString("mental-states-csv").trim()
    val detectionGoldFile = config.getString("detections-csv").trim()
    val outDir = config.getString("out-dir").trim()
    
    // Compile ground-truth XMLs
    val formatter = new MTurkFormatter(Option(indexFile))

    // For now we only support chase and hug
    if (activity.equalsIgnoreCase("chase")) {
      formatter.loadMentalStatesGold(mentalGoldFile)
      formatter.loadChaseDetectionsGold(detectionGoldFile)
    } else if (activity.equalsIgnoreCase("hug")) {
      formatter.loadMentalStatesGold(mentalGoldFile, activity = activity, subHeader = "answer.hugger",
        objHeader = "answer.huggee")
      formatter.loadHugDetectionsGold(detectionGoldFile)
    }

    // Write to XMLs
    formatter.writeGroundTruthXMLs(outDir)
  }

}
