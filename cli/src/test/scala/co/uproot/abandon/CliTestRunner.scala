package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import java.lang.Exception
import org.scalatest.StreamlinedXmlEquality._
import TestComparator._

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

class CliTestRunner extends FlatSpec with Matchers with Inside {

  "Testrunner" should "automatically test multiple dirs and confs" in {

    val testConfs =
      for (d <- FileUtils.listDirs("testCases", ".*/sclT[0-9]+(-.*)$")) yield {
        for (f <- FileUtils.listFiles(d.getAbsolutePath, ".*\\.conf$")) yield { f }
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

        // Remove old output files - these can potentially mask test failures
        Files.deleteIfExists(Paths.get(testDir.toString, output))

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
      try {
        co.uproot.abandon.CLIMain.runAppThrows(Array("-c", tc.conf) ++ tc.args)
      } catch {
        case _: Throwable => assert(false)
      }
      for (testfiles <- tc.testVec) {
        println("reference: " + testfiles.reference)
        println("output:    " + testfiles.output)

        assert(testfiles.comparator(testfiles.output, testfiles.reference))
      }
    }
  }
}
