package co.uproot.abandon

import java.io.File
import java.nio.file.FileSystems

object FileUtils {

  def listFiles(dirName: String, regex: String):Array[File] = {
    return new File(dirName).listFiles.filter({f => f.isFile && f.getCanonicalPath.matches(regex)}).sorted
  }

  def listDirs(dirName: String, regex: String):Array[File] = {
    return new File(dirName).listFiles.filter({d => d.isDirectory && d.getCanonicalPath.matches(regex)}).sorted
  }

  /*
   * In theory, this can blow up your stack, but in practice, it won't.
   * To do that, there would have to be very deep directory structure.
   */
  private def listDirTree(path: String): List[File] = {
    Option(new File(path).listFiles) match {
      case Some(files) => {
        files.toList.map { f =>
          if (f.isFile)
            List(f)
          else if (f.isDirectory)
            listDirTree(f.toString)
          else // Devices etc.
            Nil
        }.flatten
      }
      case None => Nil
    }
  }

  def globListFiles(glob: String, path: String): List[String] = {

    val matcher = FileSystems.getDefault.getPathMatcher(glob)

    listDirTree(path).filter { f => matcher.matches(f.toPath) }.map { f => f.toString() }.sorted
  }
}
