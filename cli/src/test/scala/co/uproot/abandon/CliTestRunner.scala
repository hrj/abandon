package co.uproot.abandon

import java.nio.file.{Files, Paths}

import org.scalatest.{FunSuite}

class CliTestSuite extends FunSuite {

  val testConfs =
    for (d <- FileUtils.listDirs("../testCases", ".*/sclT[0-9]+(-.*)$")) yield {
      for (f <- FileUtils.listFilesRecursive(d.getAbsolutePath, ".*\\.conf$")) yield { f }
    }

  val testCases = for (f <- testConfs.flatten) yield {
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

  for (tc <- testCases) {
    registerTest(tc.conf) {
      runTest(tc)
    }
  }

  def runTest(tc: TestCase) {
      try {
        co.uproot.abandon.CLIMain.runAppThrows(Array("-c", tc.conf) ++ tc.args ++ Array("-X", "-q"))
      } catch {
        case x: Exception => println("error: " + x); assert(false)
      }
      for (testfiles <- tc.testVec) {
        assert(testfiles.comparator(testfiles.output, testfiles.reference), "Failed comparison for " + testfiles)
      }
  }
}
