package co.uproot.abandon

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Inside

class GlobTest extends FlatSpec with Matchers with Inside {

  private def verify(paths: List[String], refPaths: List[String], baseDir: String) = {
    paths.sorted.zip(refPaths.map(f => baseDir + "/" + f).sorted).forall({
      case (path, refPath) =>
        path == refPath
    }) && !paths.isEmpty && !refPaths.isEmpty
  }

  private def printpaths(ps: List[String]) = {
    for (p <- ps) println(p)
  }

  val testDirPath = "testCases/globtree"

  "glob" should "match plain file (e.g. 'file.ext')" in {
    val refPaths = List(
      "one.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/one.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match path without glob (e.g. 'subdir/foo/file.ext')" in {
    val refPaths = List(
      "a/a.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/a/a.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match file-glob (e.g. '*.ext')" in {
    val refPaths = List(
      "one.txt",
      "two.txt",
      "three.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/*.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match sub dir file-glob  (e.g. 'subdir/*.ext')" in {
    val refPaths = List(
      "a/a.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/a/*.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match sub-directory glob with plain file (e.g. 'subdir/*/file.ext')" in {
    val refPaths = List(
      "a/x.txt",
      "c/x.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/*/x.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match sub-directory glob with file-glob (e.g. 'subdir/*/*.ext')" in {
    val refPaths = List(
      "a/a.txt",
      "a/x.txt",
      "c/c.txt",
      "c/x.txt",
      "b/b.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/*/*.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match deep sub-directory glob with plain file (e.g. 'subdir/**/file.ext')" in {
    val refPaths = List(
      "a/a2/x.txt",
      "a/x.txt",
      "c/x.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/**/x.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match deep sub-directory glob with file-glob (e.g. 'subdir/**/*.ext')" in {
    val refPaths = List(
      "a/a.txt",
      "a/x.txt",
      "a/a2/x.txt",
      "a/a2/a2.txt",
      "c/x.txt",
      "c/c.txt",
      "b/b.txt")

    val paths = FileUtils.globListFiles("glob:**/globtree/**/*.txt", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }

  it should "match everything (e.g. 'subdir/**')" in {
    val refPaths = List(
      "one.txt",
      "a/a.txt",
      "a/x.txt",
      "a/a2/x.txt",
      "a/a2/a2.txt",
      "a/a.not",
      "two.txt",
      "three.txt",
      "c/x.txt",
      "c/c.txt",
      "b/b.txt",
      "readme.md")

    val paths = FileUtils.globListFiles("glob:**/globtree/**", testDirPath)

    verify(paths, refPaths, testDirPath) should be(true)
  }
}
