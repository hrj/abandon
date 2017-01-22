package co.uproot.abandon

import java.nio.file.Path

import fi.sn127.utils.testing.DirSuiteLike

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
    val fu = fi.sn127.utils.fs.FileUtils(testname.getFileSystem)
    val testDir = fu.getParentDirPath(testname)

    val basename = fu.getBasename(testname) match { case (base, ext) => base }
    val conf = basename.toString + ".conf"

    val fullPathConf = fu.getPath(testDir.toString, conf.toString).toString

    Array("-c", fullPathConf) ++ Array("-X", "-q") ++ args
  }
}
