package co.uproot.abandon

import java.nio.file.Path

/**
  * Abandon specific default arguments for test cases where output to stdout/stderr needs to be tested as well
  */
class StdoutArgsDirSuite extends DefaultArgsDirSuite {

  /**
    * Get configuration path based on name of current test case.
    *
    * Add stdout/stderr files to the argument array mapped by DefaultArgsDirSuite
    */
  override
  protected def mapArgs(testname: Path, args: Array[String]): Array[String] = {
    val fu = fi.sn127.utils.fs.FileUtils(testname.getFileSystem)
    val testDir = fu.getParentDirPath(testname)

    val basename = fu.getBasename(testname) match { case (base, _) => base }
    val stdout = "out." + basename.toString + ".stdout.txt"
    val stderr = "out." + basename.toString + ".stderr.txt"

    val fullPathStdout = fu.getPath(testDir.toString, stdout).toString
    val fullPathStderr = fu.getPath(testDir.toString, stderr).toString

    Array(fullPathStdout, fullPathStderr) ++ super.mapArgs(testname, args)
  }
}
