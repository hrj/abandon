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

  def matcherListFiles(canonicalBasepath: String, matcher: java.nio.file.PathMatcher) = {
    val files = listDirTree(canonicalBasepath)
    files.filter { f => matcher.matches(f.toPath) }.map { f => f.toString() }.sorted
  }

  /**
   * Cook glob with basepath so that it will match relative path
   * without leading glob-pattern.
   */
  def basepathGlob(glob: String, basepath: String) = {
    // globStartChar := '*' | '?' | '{' | '[' | os.pathSep | '.'
    val pathSepRgx = if (java.io.File.separatorChar == '\\') "\\\\" else "/"
    val regex = "^glob:((\\*)|(\\?)|(\\{)|(\\[)|(" + pathSepRgx + ")|(\\.)).*"

    if (glob.matches(regex))
      // it start's with special character -> leave it alone
      glob
    else
      "glob:" + basepath + glob.stripPrefix("glob:")
  }

  def globListFiles(glob: String, basepath: String): List[String] = {
    assert(glob.startsWith("glob:"))
    val canonicalBasepath = Processor.mkCanonicalDirPath(basepath)

    val cookedGlob = basepathGlob(glob, canonicalBasepath)
    val matcher = FileSystems.getDefault.getPathMatcher(cookedGlob)
    matcherListFiles(canonicalBasepath, matcher)
  }

  def regexListFiles(regex: String, basepath: String): List[String] = {
    assert(regex.startsWith("regex:"))
    val canonicalBasepath = Processor.mkCanonicalDirPath(basepath)

    val matcher = FileSystems.getDefault.getPathMatcher(regex)

    matcherListFiles(canonicalBasepath, matcher)
  }
}
