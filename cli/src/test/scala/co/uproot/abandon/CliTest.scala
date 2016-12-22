package co.uproot.abandon

class CliTest extends DirSuite {

  /**
    * OK Cases (should succeed)
    */
  testDirSuite("../tests", ".*/sclT[0-9]+(-.*)$") { args: Array[String] =>
    assertResult(CLIApp.SUCCEEDED){
      CLIApp.mainStatus(args)
    }
  }

  /**
    * Error cases (should fail)
    */
  testDirSuite("../tests", ".*/SettingsError$") { args: Array[String] =>
    assertThrows[SettingsError]{
      CLIApp.run(args)
    }
  }
}
