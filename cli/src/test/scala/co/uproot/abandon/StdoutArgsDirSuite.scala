package co.uproot.abandon

import java.nio.file.Path
import better.files.File

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
    val testDir = File(testname).parent

    val basename = File(testname).nameWithoutExtension(true)
    val stdout = "out." + basename.toString + ".stdout.txt"
    val stderr = "out." + basename.toString + ".stderr.txt"

    val fullPathStdout = (testDir / stdout).toString
    val fullPathStderr = (testDir / stderr).toString

    Array(fullPathStdout, fullPathStderr) ++ super.mapArgs(testname, args)
  }
}
