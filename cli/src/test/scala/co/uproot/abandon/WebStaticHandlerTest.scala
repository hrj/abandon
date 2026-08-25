package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Paths

class WebStaticHandlerTest extends AnyFlatSpec with Matchers {

  "Path normalization" should "prevent path traversal out of the build directory" in {
    val basePath = Paths.get("build").normalize()

    val normalPath = basePath.resolve("index.html").normalize()
    normalPath.startsWith(basePath) should be(true)

    val traversalPath1 = basePath.resolve("../App.scala").normalize()
    traversalPath1.startsWith(basePath) should be(false)

    val traversalPath2 = basePath.resolve("../../etc/passwd").normalize()
    traversalPath2.startsWith(basePath) should be(false)

    val traversalPath3 = basePath.resolve("sub/../../secret.txt").normalize()
    traversalPath3.startsWith(basePath) should be(false)
  }
}
