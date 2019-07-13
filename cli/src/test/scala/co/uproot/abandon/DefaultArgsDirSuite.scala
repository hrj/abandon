package co.uproot.abandon

import java.nio.file.Path
import better.files.File

import fi.e257.testing.DirSuiteLike

/**
 * Abandon specific default arguments for test cases.
 */
class DefaultArgsDirSuite extends DirSuiteLike  {

  /**
   * Get configuration path based on name of current
   * test case.
   *
   * Add conf-file and few other extra arguments to all
   * test executions calls.
   */
  override
  protected def mapArgs(testname: Path, args: Array[String]): Array[String] = {
    val testDir = File(testname).parent

    val basename = File(testname).nameWithoutExtension(true)
    val conf = basename.toString + ".conf"

    val fullPathConf = (testDir / conf).toString()

    Array("-c", fullPathConf) ++ Array("-X", "-q") ++ args
  }
}
