package co.uproot.abandon

import java.io.FileOutputStream
import java.nio.file.Paths

import fi.sn127.utils.fs.Glob

/**
  * Test valid and invalid input with full CLI App
  * and verify return value of main app.
  */
class CliAppTests extends DefaultArgsDirSuite {
  val basedir = Paths.get("../tests").toAbsolutePath.normalize


  /**
    * OK Cases (should succeed)
    */
  runDirSuiteTestCases(basedir, Glob("sclT*/**.exec")) { args: Array[String] =>
    assertResult(CLIApp.SUCCEEDED) {
      CLIApp.mainStatus(args)
    }
  }

  /**
    * Error cases (should fail)
    * Pending bugs are kept inside bugs* so don't match that
    */
  runDirSuiteTestCases(basedir, Glob("errors/[A-Z]*/**.exec")) { args: Array[String] =>
    assertResult(CLIApp.FAILED) {
      CLIApp.mainStatus(args)
    }
  }
}

class CliStdoutTests extends StdoutArgsDirSuite {
  val basedir = Paths.get("../tests").toAbsolutePath.normalize

  /**
    * OK Cases with stdout/stderr output (should succeed)
    */
  runDirSuiteTestCases(basedir, Glob("sclX*/**.exec")) { args: Array[String] =>
    assertResult(CLIApp.SUCCEEDED) {
      Console.withOut(new FileOutputStream(args(0))) {
        Console.withErr(new FileOutputStream(args(1))) {
          CLIApp.mainStatus(args.drop(2))
        }
      }
    }
  }
}

/**
  * Test invalid input and options with CliApp
  * and verify that specific exception is thrown.
  */
class CliAppErrors extends DefaultArgsDirSuite {
  val errorRoot = Paths.get("../tests/errors").toAbsolutePath.normalize

  /*
   * OK errors follows
   */

  runDirSuiteTestCases(errorRoot, Glob("InputFileNotFoundError/**.exec")) { args: Array[String] =>
    assertThrows[InputFileNotFoundError] {
      CLIApp.run(args)
    }
  }

  runDirSuiteTestCases(errorRoot, Glob("InputError/**.exec")) { args: Array[String] =>
    assertThrows[InputError] {
      CLIApp.run(args)
    }
  }

  runDirSuiteTestCases(errorRoot, Glob("SettingsError/**.exec")) { args: Array[String] =>
    assertThrows[SettingsError] {
      CLIApp.run(args)
    }
  }

  runDirSuiteTestCases(errorRoot, Glob("ConstraintError/**.exec")) { args: Array[String] =>
    assertThrows[ConstraintError] {
      CLIApp.run(args)
    }
  }
}
