package co.uproot.abandon

/**
  * Test valid and invalid input with full CLI App
  * and verify return value of main app.
  */
class CliAppTests extends DirSuite {

  /**
    * OK Cases (should succeed)
    */
  testDirSuite("../tests", ".*/sclT[0-9]+(-.*)$") { args: Array[String] =>
    assertResult(CLIApp.SUCCEEDED) {
      CLIApp.mainStatus(args)
    }
  }

  /**
    * Error cases (should fail)
    * Pending bugs are kept inside bugs* so don't match that
    */
  testDirSuite("../tests/errors", ".*/[A-Z]([a-zA-Z])+$") { args: Array[String] =>
    assertResult(CLIApp.FAILED) {
      CLIApp.mainStatus(args)
    }
  }
}

/**
  * Test invalid input and options with CliApp
  * and verify that specific exception is thrown.
  */
class CliAppErrors extends DirSuite {

  val errorRoot = "../tests/errors"

  /**
    * these should fail, but they are not failing
    */
  ignoreDirSuite(errorRoot, ".*/bugsInputError$") { args: Array[String] =>
    assertThrows[SettingsError] {
      CLIApp.run(args)
    }
  }
  /**
    * These errors should be specialized, but at least they error out at the moment
    */
  testDirSuite(errorRoot, ".*/AssertionError$") { args: Array[String] =>
    assertThrows[AssertionError] {
      CLIApp.run(args)
    }
  }

  /*
   * OK errors follows
   */

  testDirSuite(errorRoot, ".*/InputFileNotFoundError$") { args: Array[String] =>
    assertThrows[InputFileNotFoundError] {
      CLIApp.run(args)
    }
  }

  testDirSuite(errorRoot, ".*/InputError$") { args: Array[String] =>
    assertThrows[InputError] {
      CLIApp.run(args)
    }
  }

  testDirSuite(errorRoot, ".*/SettingsError$") { args: Array[String] =>
    assertThrows[SettingsError] {
      CLIApp.run(args)
    }
  }

  testDirSuite(errorRoot, ".*/ConstraintError$") { args: Array[String] =>
    assertThrows[ConstraintError] {
      CLIApp.run(args)
    }
  }
}
