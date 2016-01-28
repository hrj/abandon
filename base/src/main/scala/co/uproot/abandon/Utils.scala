package co.uproot.abandon

import java.io.File

object FileUtils {

  def listFiles(dirName: String, regex: String):Array[File] = {
    return new File(dirName).listFiles.filter({f => f.isFile && f.getCanonicalPath.matches(regex)}).sorted
  }

  def listDirs(dirName: String, regex: String):Array[File] = {
    return new File(dirName).listFiles.filter({d => d.isDirectory && d.getCanonicalPath.matches(regex)}).sorted
  }

}
