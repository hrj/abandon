package co.uproot.abandon

import org.scalatest.{FlatSpec, Matchers}

class CLIMainTest extends FlatSpec with Matchers {

  behavior of "Abandon CLI with no-op args"
  it should "handle version" in {
    co.uproot.abandon.CLIApp.run(Array("--version"))
  }
  it should "handle -h by throwing the Help exception" in {
    an [org.rogach.scallop.exceptions.Help] should be thrownBy {
      co.uproot.abandon.CLIApp.run(Array("-h"))
    }
  }
}
