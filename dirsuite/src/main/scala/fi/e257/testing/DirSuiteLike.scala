/*
 * Copyright 2016-2019 E257.FI
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package fi.e257.testing

import java.nio.file.Path

import better.files._
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

import scala.collection.immutable

/*
class File(path: java.nio.file.Path) {
  def this(name: String) = this(java.nio.file.Paths.get(name))

  def lines: Seq[String] = ???

  def nameWithoutExtension(includeAll: Boolean = false) = ???
}
*/

/**
 * Generic exception class for DirSuite. This is typically thrown in
 * cases when there is something broken about dirsuite setup.
 *
 * @param msg message
 * @param cause if there was underlying exception
 */
class DirSuiteException(msg: String, cause: Option[Throwable]) extends
  TestFailedException(
    { (_: StackDepthException) => Some(msg) },
    cause,
    Left[Position, StackDepthException => Int](Position("", "", 0)),
    None,
    immutable.IndexedSeq.empty[String]
  )

/**
 * Exception class for testvector validation errors. Validator is NOT supposed
 * throw this, but to use Option[String] to report test vector failures.
 *
 * Typically this is thrown in case that output files is not found, or there was
 * an exception because of content of output (output is not syntactically correct,
 * e.g. JSON/XML SAX errors).
 *
 * @param msg message
 * @param cause if there was underlying exception
 */
class TestVectorException(msg: String, cause: Option[Throwable]) extends
  TestFailedException(
    { (_: StackDepthException) => Some(msg) },
    cause,
    Left[Position, StackDepthException => Int](Position("", "", 0)),
    None,
    immutable.IndexedSeq.empty[String]
  )

/**
 * Eech test vector contains one input/output pair and validator-method to validate
 * this testvector.  One testcase con contain zero or many testvectors.
 *
 * @param reference expected output
 * @param output actual output
 * @param validator for this testvector
 */
final case class TestVector(reference: Path, output: Path, validator: (Path, Path, Path) => Option[String]) {

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def makeComparatorErrMsg(prefix: String, tc: TestCase) : String = {
    prefix + "\n" +
      "   with name: [" + tc.name + "]\n" +
      "   with execution sequence:\n" +
      tc.execsToString(" " * 6 + "exec") + "\n" +
      "   failed test vector (output) after successful executions is: \n" +
      "     reference: [" + reference.toString + "]\n" +
      "     output:    [" + output.toString + "]"
  }
}

/**
 * This holds all information about one test case
 *
 * @param testname full path of test's exec-file.
 * @param execs sequence of steps which will be executed when this test case is run.
 * @param testVectors test vectors. These are checked after all steps are executed.
 */
@SuppressWarnings(Array("org.wartremover.warts.ToString"))
final case class TestCase(testname: Path, execs: Seq[Array[String]], testVectors: Seq[TestVector]){
  val name: String = testname.toString
  val testPath: Path = testname

  /**
   * Reporting: execs to pretty string
   * @param prefix for each exec line
   * @return execs as pretty string
   */
  def execsToString(prefix: String): String = {
      execs.zipWithIndex
      .map({ case (args, idx) =>
        prefix + " %d:".format(idx) + " [" + args.mkString("", ",", "") + "]"
      }).mkString("\n")
  }

  /**
   * Reporting: Failure message in case execution of one step failed
   * @param prefix for whole error message
   * @param idx  index of which step failed
   * @param realArgs actual arguments which were used to execute failing step ([[DirSuite.mapArgs]])
   * @return error message as pretty string
   */
  def makeExecFailMsg(prefix: String, idx: Int, realArgs: Array[String]) : String = {
    prefix + "\n" +
      "   name: " + name + "]\n" +
      "   with execution sequence:\n" +
      execsToString(" " * 6 + "exec") + "\n" +
      "   actual failed execution is: \n" +
      realArgs.mkString(" " * 6 + "exec " + "%d:".format(idx) + " [", ",", "]") + "\n"
  }
}

object DirSuiteLike {
  val executionFailureMsgPrefix = "TEST FAILED WITH UNEXPECTED EXECUTION RESULT!"
  val testVectorFailureMsgPrefix = "TEST FAILED WITH UNEXPECTED TEST VECTOR RESULT!"
  val testVectorExceptionMsgPrefix = "TEST FAILED WITH EXCEPTION WHILE COMPARING TEST VECTORS!"
}

/**
 * DirSuiteLike trait for DirSuite.
 * At the moment this is actual implementation.
 */
@SuppressWarnings(Array(
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.NonUnitStatements"))
trait DirSuiteLike extends AnyFunSuiteLike {

  /**
   * Get separator for exec line splitting.
   *
   * This separator is used to split one exec line to actual arguments.
   * Overload this if default ";" is not good for you.
   *
   * @return exec line separator
   */
  protected def getExecArgumentSeparator: String = ";"

  /**
   * Tokenizer for exec-line.
   *
   * If default separator for arguments is not good for you,
   * then overload [[getExecArgumentSeparator]].
   *
   * If you have to use some other format for exec lines (e.g.) JSON,
   * then overload this function. If one raw: exec line is:
   *
   * exec := "exec:" TEXT
   *
   * then this function will be fed with TEXT.
   *
   * @param execLine one exec line which to split, with "exec:" prefix stripped away.
   * @return execline as  args vector
   */
  protected def tokenizer(execLine: String): Array[String] = {
    // TODO: Fix this silly error handling
    if (execLine.isEmpty) {
      // No args-case => ok
      Array[String]()
    } else {
      val rawArgs = execLine.split(getExecArgumentSeparator, -1)

      if (rawArgs.size < 2) {
        // TODO: Exception, not nice
        throw new DirSuiteException("Exec line is not terminated with ';': [" + execLine + "]", None)
      } else {
        val sgrAwar = rawArgs.reverse
        val last = sgrAwar.head
        if (last.nonEmpty) {
          // there is trailing stuff after last ';',
          // so it could be missed semi-colon
          // TODO: Exception, not nice
          throw new DirSuiteException("Exec line is not terminated with ';': [" + execLine + "]", None)
        } else {
          // drop "last", and return list in correct order
          sgrAwar.drop(1).reverse
        }
      }
    }
  }

  /**
   * Parse one exec file.
   *
   * If you have to change exec file format, look for
   * [[tokenizer]] to parse each exec line and
   * [[getExecArgumentSeparator]] to change
   * exec's arguments separator.
   *
   * @param testname full path of testcase
   * @return exec steps and arguments for each step
   */
  protected def parseExec(testname: Path): Seq[Array[String]] = {
    val exexPrefix = "exec:"

    File(testname).lines
      .filter(!_.startsWith("#"))
      .filter(_.startsWith(exexPrefix))
      .map(_.stripPrefix(exexPrefix))
      .map(str => str.trim)
      .map(tokenizer)
      .toSeq
  }

  /**
   * Map arguments before test execution.
   * If you have to provide e.g. some default argument for each test case,
   * then overload this function, and change arguments as needed.
   * (Add, remove, or transform args).
   *
   * @param testname full path of testcase
   * @param args original arguments
   * @return args after transformation
   */
  protected def mapArgs(testname: Path, args: Array[String]): Array[String] = {
    args
  }

  /**
   * Find references for testcase.
   *
   * Default logic to find references is following:
   *
   * use basename of testcase, e.g. filename of testcase without last extension,
   * and append ".ref.*" glob to that basename. Then search under same directory,
   * where testcase is located.
   *
   * For example:
   *
   *    /path/to/dirsuite/testcase/test01.exec
   *
   *    findFiles("/path/to/dirsuite/testcase", Glob("test01.ref.*"))
   *
   * @param testdir top level directory from where to look
   * @param testname
   * @return
   */
  protected def findReferences(testdir: Path, testname: Path): Seq[Path] = {
    // TODO Check this logic (e.g. should File's testdir achor be based on testname?
    val basename = File(testname).nameWithoutExtension(true)

    File(testdir)
      .glob(basename + ".ref.*")(visitOptions = File.VisitOptions.follow)
      .map(f => f.path)
      .toSeq
      .sorted
  }

  /**
   * Map output filenames based on reference paths.
   *
   * use basename of testcase, e.g. filename of testcase without last extension,
   * and prefix "out.*" to that basename. Then append reference's filename suffix
   * to that name. Reference file suffix is stripped (basename + ".ref.").
   *
   * For example:
   *
   *    /path/to/dirsuite/testcase/test01.exec
   *    /path/to/dirsuite/testcase/test01.ref.output-name.txt
   *
   *    output : out.test01.output-name.txt
   *
   * @param testdir directory of this test case
   * @param testname full path to test case
   * @param reference full path to reference file
   * @return full path to mapped output file path
   */
  protected def mapOutput(testdir: Path, testname: Path, reference: Path): Path = {

    val basename = File(testname).nameWithoutExtension(true)

    val output = "out." + basename + "." + reference.getFileName.toString.stripPrefix(basename + ".ref.")
    (File(testdir) / output).path
  }

  /**
   * Select validator for this (one) test vector.
   *
   * Default logic is that [[TestValidator.txtValidator]] is used
   * for "*.txt" files, and [[TestValidator.xmlValidator]] is used for
   * "*.xml" files.
   *
   * @param testname full path of this test case
   * @param reference full path of reference file
   * @param output full path of output file
   * @return Validator for this test vector
   */
  protected def selectValidator(testname: Path, reference: Path, output: Path): ((Path, Path, Path) => Option[String]) = {

    val refExt = File(reference).extension(includeDot = false, includeAll = false, toLowerCase = true)
    refExt match {
      case Some("txt") => TestValidator.txtValidator
      case Some("xml") => TestValidator.xmlValidator
      case _ => TestValidator.txtValidator
    }
  }

  /**
   * Register dirsuite
   *
   * If you don't like see each test case to be printed on test output,
   * you could override this function and just call [[testCaseExecutor]] here.
   *
   * @param pattern which was used to find current test case
   * @param tc test case, see [[TestVector]] for available information
   * @param testFuns test functions which are run with this test case
   */
  protected def registerDirSuiteTestCase(
    pattern: FindFilesPattern,
    tc: TestCase,
    testFuns: List[(Array[String]) => Any]): Unit = {

    registerTest(pattern.toString + " => " + tc.name.toString) {
      testCaseExecutor(tc, testFuns)
    }
  }

  /**
   * Register ignored test case.
   *
   * If you don't like see ignored test case to be printed on test output,
   * you can overload this function and just don't do anything.
   * Or even better, overload [[ignoreDirSuiteTestCases]] and register whole dirSuite once,
   * so you still get information that there are ignored tests.
   *
   * @param pattern which was used to find current test case
   * @param testname full path to test case
   */
  protected def registerIgnoredDirSuiteTestCase(pattern: FindFilesPattern, testname: Path): Unit = {
    registerIgnoredTest(pattern.toString + " => " + testname.toString) {}
  }

  private def findFiles(basedir: Path, testPattern: FindFilesPattern): Seq[File] = {
    testPattern match {
      case glob: Glob => File(basedir).glob(glob.glob).toSeq.sortBy(_.path)
      case regex: Regex => File(basedir).globRegex(regex.regex.r).toSeq.sortBy(_.path)
    }
  }

  /**
   * Ignore these dirSuite tests.
   *
   * Overload this if youd don't like to see all testcases which are ignored.
   * You could here just cal [[registerIgnoredTest]] and register e.g. pattern.
   *
   * @param basedir of test cases
   * @param testPattern pattern to find test cases
   * @param testFun test function to run with test cases
   */
  def ignoreDirSuiteTestCases(basedir: Path, testPattern: FindFilesPattern)(testFun: (Array[String] => Any)): Unit = {

    findFiles(basedir, testPattern)
      .foreach(test =>
        registerIgnoredDirSuiteTestCase(testPattern, test.path)
      )
  }

  /**
   * Ignore these dirSuite tests.
   *
   * Overload this if youd don't like to see all testcases which are ignored.
   * You could here just cal [[registerIgnoredTest]] and register e.g. pattern.
   *
   * @param basedir of test cases
   * @param testPattern pattern to find test cases
   * @param beginTestFun test function which is used 1 .. N-1 test steps
   * @param lastTestFun test function which is used for the last test step
   */
  def ignoreDualAssertionDirSuiteTestCases(basedir: Path, testPattern: FindFilesPattern)(
    beginTestFun: (Array[String] => Any),
    lastTestFun: (Array[String] => Any)): Unit = {

    findFiles(basedir, testPattern)
      .foreach(test =>
        registerIgnoredDirSuiteTestCase(testPattern, test.path)
      )
  }

  /**
   * Find test cases under basedir, by using test pattern
   * to match test cases.
   *
   * @param basedir of test directory
   * @param testPattern pattern to find test cases
   * @return found test cases
   */
  protected def getDirSuiteTestCases(basedir: Path, testPattern: FindFilesPattern): Seq[TestCase] = {

    val td = File(basedir)
    if (!td.isDirectory || td.isEmpty) {
      throw new DirSuiteException("=>\n" +
        " " * 3 + "The basedir for DirSuite is invalid\n" +
        " " * 6 + "basedir: [" + basedir.toString + "]\n",
        None)
    }

    val testnames = findFiles(basedir, testPattern)

    if (testnames.isEmpty) {
      throw new DirSuiteException("=>\n" +
        " " * 3 + "DirSuite test set is empty - there are no exec-files!\n" +
        " " * 6 + "basedir: [" + basedir.toString + "]\n" +
        " " * 6 + "pattern: " + testPattern.toString + "\n" +
        " " * 3 + "if this is intentional, then change test set to be ignored (run -> ignore).",
        None)
    }

    val testCases = testnames.map(_.path).map(testname => {
      val testdir = testname.getParent

      val execs = parseExec(testname)
      if (execs.isEmpty) {
        throw new DirSuiteException("=>\n" +
          " " * 3 + "Exec for test is empty - there is nothing to run!\n" +
          " " * 6 + "basedir: [" + basedir.toString + "]\n" +
          " " * 6 + "testname: [" + testname.toString + "]\n",
          None)
      }

      val testVectors = findReferences(testdir, testname)
        .map(reference => {
          val output = mapOutput(testdir, testname, reference)
          val comparator = selectValidator(testname, reference, output)

          TestVector(reference, output, comparator)
        })

      TestCase(testname, execs, testVectors)
    })
    testCases.toSeq
  }

  /**
   * Find and Run test cases based on basedir and testPattern.
   *
   * In addition of normal parameter, there is actual test method as parameter.
   * This test method will be executed for each exec line, with arguments wich
   * are defined by that exec-line.
   *
   * Below is listed typical test case:
   * {{{
   *   runDirSuiteTestCases(testdir, Glob("success/basic[0-9]*.exec")) { args: Array[String] =>
   *     assertResult(DemoApp.SUCCESS) {
   *       app.doTxt(args)
   *     }
   *   }
   * }}}
   *
   * @param basedir of test case directory
   * @param testPattern pattern of test cases
   * @param testFun test function which is used to test each test case
   */
  def runDirSuiteTestCases(basedir: Path, testPattern: FindFilesPattern)(testFun: (Array[String] => Any)): Unit = {

    getDirSuiteTestCases(basedir, testPattern).foreach(tc => {
      registerDirSuiteTestCase(testPattern, tc, List[(Array[String] => Any)](testFun))
    })
  }

  /**
   * Run (and find) dual assertion test cases.
   * These are typically test cases where first steps
   * are supposed to behave one way (succeeds),
   * and then the last step is supposed to behave differently (fails).
   *
   * This is useful for example testing a case when
   * triggering of error you must first run multiple successful steps.
   *
   * {{{
   *   runDualAssertionDirSuiteTestCases(testdir, Glob("success/multiStepFail[0-9]*.exec"))
   *     { args: Array[String] =>
   *       // All steps at first must succeed
   *       assertResult(DemoApp.SUCCESS) {
   *         app.doFlaky(args)
   *       }
   *     } { args: Array[String] =>
   *         // Then the last step must fail
   *         assertThrows[RuntimeException] {
   *           app.doFlaky(args)
   *       }
   *     }
   * }}}
   *
   * @param basedir of test case directory
   * @param testPattern Glob or Regex pattern of test cases
   * @param firstTestFun test function for first execution steps
   * @param lastTestFun  test function for last execution step
   */
  def runDualAssertionDirSuiteTestCases(basedir: Path, testPattern: FindFilesPattern)
    (firstTestFun: (Array[String] => Any))
    (lastTestFun: (Array[String] => Any)): Unit = {

    val testcases = getDirSuiteTestCases(basedir, testPattern)

    testcases.foreach(tc => {
      registerDirSuiteTestCase(testPattern, tc, List[(Array[String] => Any)](firstTestFun, lastTestFun))
    })
  }


  /**
   * Execute one test case.
   *
   * @param tc test case to be executed
   * @param testFuns test functions to be used for testing
   */
  @SuppressWarnings(Array(
    "org.wartremover.warts.Any",
    "org.wartremover.warts.TraversableOps"))
  protected def testCaseExecutor(tc: TestCase, testFuns: List[(Array[String] => Any)]) = {

    if (tc.execs.length < testFuns.length) {
      throw new DirSuiteException("=>\n" +
        " " * 3 + "Exec line count is less than test function count. This is not supported!\n" +
        " " * 6 + "testname: " + tc.testname.toString + "\n",
        None)
    }
    /*
      Bake execs with testfuns

      val funs = List("a", "last")
      val is = List(1, 2, 3, 4, 5)
      is.reverse.zipAll(funs.reverse, 0, funs.head).reverse
      res1: List[(Int, String)] = List((1,a), (2,a), (3,a), (4,a), (5,last))
     */
    val execsAndFunc: Seq[(Array[String], (Array[String]) => Any)] = tc.execs
      .reverse
      .zipAll(testFuns.reverse, Array[String](), testFuns.head)
      .reverse

    execsAndFunc.zipWithIndex.foreach({
      case ((args, testFun), index) =>

        val execArgs = mapArgs(tc.testname, args)
        try {

          /* this is real-deal for test run */
          testFun(execArgs)

        } catch {
          case tfe: TestFailedException =>
            throw tfe.modifyMessage(origMsg => {
              Option("" +
                tc.makeExecFailMsg(DirSuiteLike.executionFailureMsgPrefix, index, execArgs) +
                " " * 3 + "Failed result: \n" +
                " " * 6 + origMsg.getOrElse("") + "\nPosition: ")
            })
          case ex: Exception => {
            val msg = "" +
              tc.makeExecFailMsg(DirSuiteLike.executionFailureMsgPrefix, index, execArgs) +
              " " * 3 + "Exception: \n" +
              " " * 6 + ex.getClass.getName + "\n" +
              " " * 3 + "Message: \n" +
              " " * 6 + ex.getMessage + "\n" +
              " " * 3 + "Cause: \n" +
              " " * 6 + ex.getStackTrace.toSeq.map(e => e.toString).take(3).mkString("", "\n" + " " * 6, "\n" + " " * 6 + "...\nPosition: ")

            throw new DirSuiteException(msg, Some(ex))
          }
        }
    })

    /*
     * validate test vectors
     */
    tc.testVectors.foreach({ testVector =>
      val compErrorMsg: Option[String] = try {

        /* this is real deal for test result validation */
        val compResult = testVector.validator(tc.testname, testVector.reference, testVector.output)

        compResult.map(msg =>
          testVector.makeComparatorErrMsg(DirSuiteLike.testVectorFailureMsgPrefix, tc) + "\n" +
            " " * 3 + "Comparator: \n" +
            " " * 6 + "msg: " + msg + "\nPosition: "
        )
      } catch {
        case ex: Exception => Some(
          testVector.makeComparatorErrMsg(DirSuiteLike.testVectorExceptionMsgPrefix, tc) + "\n" +
            " " * 3 + "Exception: \n" +
            " " * 6 + "cause: " + ex.getClass.getCanonicalName + "\n" +
            " " * 6 + "msg: " + ex.getMessage + "\nPosition: "
        )
      }
      // NOTE: Collect all comp results, and report all end results together (Cats ...)?
      compErrorMsg.foreach(s => {
        throw new TestVectorException(s, None)
        () // Inferred type containing Nothing
      })
    })
  }
}
