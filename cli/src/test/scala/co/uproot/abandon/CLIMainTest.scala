package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CLIMainTest extends AnyFlatSpec with Matchers {

  behavior of "Abandon CLI with no-op args"
  it should "handle version" in {
    co.uproot.abandon.CLIApp.run(Array("--version"))
  }
  it should "handle h" in {
    co.uproot.abandon.CLIApp.run(Array("-h"))
  }
  it should "handle help" in {
    co.uproot.abandon.CLIApp.run(Array("--help"))
  }
  it should "handle an unknown option by throwing an UnknownOption exception" in {
    an[org.rogach.scallop.exceptions.UnknownOption] should be thrownBy {
      co.uproot.abandon.CLIApp.run(Array("--unknown"))
    }
  }
}
