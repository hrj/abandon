package co.uproot.abandon

import java.io.File
import java.nio.file.{Files, Path, Paths}

import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException

trait DirSuite extends FunSuiteLike {

  /**
    * Check if path exists and err-out with exception if not
    *
    * @param path  to be checked
    * @return real path to basedir
    */
  protected def ensurePath(path: String): Path = {
    Paths.get(path).toRealPath()
  }

  protected def findTestConf(realPath: Path, dirPattern: String): Array[File] = {
    val confs = for (d <- FileUtils.listDirs(realPath.toString, dirPattern)) yield {
      for (f <- FileUtils.listFilesRecursive(d.getAbsolutePath, ".*\\.conf$")) yield {
        f
      }
    }
    confs.flatten
  }

  /**
    * Ignores all tests in directory suite.
    *
    * @param basedir is top level directory for directory suite
    * @param dirPattern  is regexp which defines a path pattern for test directories
    * @param specimen specimen, this is not used
    */
  def ignoreDirSuite(basedir: String, dirPattern: String)(specimen: (Array[String] => Any)): Unit = {
    val realPath = ensurePath(basedir)

    findTestConf(realPath, dirPattern).foreach(path => {
      registerIgnoredTest(dirPattern + " => " + path.toString){}
    })
  }

  /**
    * Registers and run all tests in directory suite.
    *
    * @param basedir is top level directory for directory suite
    * @param dirPattern  is regexp which defines a path pattern for test directories
    * @param specimen should run App and signal if result was expected
    */
  def testDirSuite(basedir: String, dirPattern: String)(specimen: (Array[String] => Any)) {

    val realPath = ensurePath(basedir)

    val testConfs = findTestConf(realPath, dirPattern)

    val tests = for (f <- testConfs) yield {
      val testDir = f.toPath.getParent
      val basename = f.toPath.getFileName.toString.stripSuffix(".conf")

      val argsPath = Paths.get(testDir.toString, basename + ".args")
      val args =
        if (Files.isReadable(argsPath)) {
          val source = io.Source.fromFile(argsPath.toString)
          try source.getLines.map(str => str.trim).toArray finally source.close
        } else {
          Array[String]()
        }

      val refFiles = FileUtils.listFiles(testDir.toString, ".*/" + basename + "\\.ref\\..*")

      val testVecs = for (rf <- refFiles) yield {
        val ref = rf.toPath.getFileName.toString()
        val output = "out." + basename + "." + ref.stripPrefix(basename + ".ref.")
        /*
         * Old output files - these can potentially mask test failures
         * Let's err on safe side and not delete files for now.
         * TODO: Use conditionally in memory file system for output
         */

        if (ref.endsWith(".xml")) {
          TestVec(testDir.toString + "/" + output,
            testDir.toString + "/" + ref, TestComparator.xmlComparator)
        } else {
          TestVec(testDir.toString + "/" + output,
            testDir.toString + "/" + ref, TestComparator.txtComparator)
        }
      }
      TestCase(f.toPath().toString, args, testVecs.toList)
    }

    for (tc <- tests) {
      registerTest(tc.conf) {
        runTest(tc)
      }
    }

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    def runTest(tc: TestCase) {
      val allArgs = Array("-c", tc.conf) ++ tc.args ++ Array("-X", "-q")
      try {
        specimen(allArgs)
      } catch {
        case tfe: TestFailedException =>
          val execFailEx = tfe.modifyMessage(origMsg => {
            Option("\n" +
              "UNEXPECTED EXEC RESULT - TEST FAILED!\n" +
              "   with conf: " + tc.conf.toString + "\n" +
              "   with args: " +
              allArgs.mkString("\"", "\" \"", "\"") + "\n" +
              "REASON: \n" +
              "   " + origMsg.getOrElse(""))
          })
          throw execFailEx
      }

      for (testFiles <- tc.testVec) {
        def filesMsg = {
          "  conf: " + tc.conf.toString + "\n" +
            "   ref: " + testFiles.reference + "\n" +
            "   out: " + testFiles.output + "\n"
        }

        // prettify possible XML/SAX/etc. exceptions
        val cmpResult = try {
          testFiles.comparator(testFiles.output, testFiles.reference) match {
            case true => None
            case false => Some("\n" +
              "END RESULT DIFFERS WITH REF!\n" +
              filesMsg +
              "TEST FAILED\n"
            )
          }
        } catch {
          case ex: Exception => Some("\n" +
              "EXCEPTION WHILE COMPARING RESULTS:\n" +
              filesMsg +
              "TEST FAILED\n" +
              "Exception: " + ex.getMessage)
        }

        cmpResult match {
          case Some(errMsg) => fail(errMsg)
          case None =>
        }
      }
    }
  }
}
