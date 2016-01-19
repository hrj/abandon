package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.Matchers
import org.scalatest.Inside
import java.lang.Exception
import org.scalatest.StreamlinedXmlEquality._
import TestComparator._

import java.io.File

class CliTestRunner extends FlatSpec with Matchers with Inside {

	"Testrunner" should "automatically test multiple dirs and confs" in {
	  
	  val testConfs = 
	      for (d <- FileUtils.listDirs("testCases", ".*/sclT[0-9]+(-.*)$")) yield {  
	        for (f <- FileUtils.listFiles(d.getAbsolutePath, ".*\\.conf$")) 
	          yield { f } 
	      }
	  
	  val testCases = for (f <- testConfs.flatten) yield {
	      val testDir = f.toPath.getParent
	      val basename = f.toPath.getFileName.toString.stripSuffix(".conf")
	      val refFiles = FileUtils.listFiles(testDir.toString , ".*/" + basename + "\\.ref\\..*")
	      
	      val testVecs = for (rf <- refFiles) yield {
	        val ref = rf.toPath.getFileName.toString()
	        val output = "out." + basename + "." + ref.stripPrefix(basename + ".ref.")
	        
	        if (ref.endsWith(".xml"))
	          TestVec(testDir.toString + "/" + output,
	               testDir.toString + "/" + ref, TestComparator.xmlComparator)
	        else 
	          TestVec(testDir.toString + "/" + output,
	              testDir.toString + "/" + ref, TestComparator.txtComparator)	        
	      }
	      TestCase(f.toPath().toString, testVecs.toList)
	  }
	  
	  for (tc <- testCases) {
	   
            try {
	       co.uproot.abandon.CLIMain.runAppThrows(Array("-c", tc.conf))
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
