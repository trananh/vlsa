package edu.arizona.sista.vlsa.utils

import java.io.File
import java.io.IOException
import scala.Array.canBuildFrom
import scala.util.matching.Regex

/** Provides some common IO utilities.
  * 
  * @author trananh
  */
object IOUtils {

  /** Recursively retrieves '''all''' files from the current folder, as well
    * as from all sub-folders.
    *
    * @param dir The current directory.
    * @param f Some filtering function.
    * @return All files found.
    */
  @throws(classOf[IOException])
  def getAllFilesRecursive(dir: File, f: File => Boolean): Array[File] = {
    val these = dir.listFiles
    if (these == null)
      return Array[File]()
    val good = these.filter(f)
    good ++ these.filter(_.isDirectory).flatMap(getAllFilesRecursive(_, f))
  }

  /** Recursively retrieves '''all''' files from the current folder, as well
    * as from all sub-folders.
    *
    * ''NOTE:'' Only files of type "File" are returned (i.e., no directories).
    *
    * @param dir The current directory.
    * @param r Some filtering regular expression pattern.
    * @return All files found.
    */
  @throws(classOf[IOException])
  def getAllFilesRecursive(dir: File, r: Regex): Array[File] = {
    getAllFilesRecursive(dir, f => f.isFile && r.findFirstIn(f.getName).isDefined)
  }

  /** Recursively retrieves '''all''' files from the current folder, as well
    * as from all sub-folders.
    *
    * ''NOTE:'' Only files of type "File" are returned (i.e., no directories).
    *
    * @param dir The current directory.
    * @return All files found.
    */
  @throws(classOf[IOException])
  def getAllFilesRecursive(dir: File): Array[File] = {
    getAllFilesRecursive(dir, f => f.isFile)
  }
  
}