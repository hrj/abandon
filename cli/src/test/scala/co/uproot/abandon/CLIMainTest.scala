package co.uproot.abandon

import org.scalatest.FlatSpec

class CLIMainTest extends FlatSpec {

  behavior of "Abandon CLI with no-op args"
  it should "handle version" in {
    co.uproot.abandon.CLIMain.run(Array("--version"))
  }
}
