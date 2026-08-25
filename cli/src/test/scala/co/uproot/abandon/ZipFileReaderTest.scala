package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream, ZipInputStream}

class ZipFileReaderTest extends AnyFlatSpec with Matchers {

  private def createZipStream(entries: Seq[(String, Array[Byte])]): ZipInputStream = {
    val baos = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(baos)
    for ((name, data) <- entries) {
      val entry = new ZipEntry(name)
      zos.putNextEntry(entry)
      zos.write(data)
      zos.closeEntry()
    }
    zos.close()
    new ZipInputStream(new ByteArrayInputStream(baos.toByteArray))
  }

  "ZipFileReader" should "successfully read valid zip entries" in {
    val entries = Seq(
      "build/index.html" -> "<html></html>".getBytes("UTF-8"),
      "build/static/js/app.js" -> "console.log('hello');".getBytes("UTF-8")
    )
    val zis = createZipStream(entries)
    val fileMap = ZipFileReader.readZipFile(zis)

    fileMap should contain key "build/index.html"
    fileMap should contain key "build/static/js/app.js"
    new String(fileMap("build/index.html"), "UTF-8") shouldBe "<html></html>"
  }

  it should "normalize windows-style path separators in zip entries" in {
    val entries = Seq(
      "build\\index.html" -> "<html></html>".getBytes("UTF-8")
    )
    val zis = createZipStream(entries)
    val fileMap = ZipFileReader.readZipFile(zis)

    fileMap should contain key "build/index.html"
  }

  it should "throw IllegalArgumentException when a zip entry contains path traversal with .." in {
    val entries = Seq(
      "../etc/passwd" -> "malicious".getBytes("UTF-8")
    )
    val zis = createZipStream(entries)

    an [IllegalArgumentException] should be thrownBy {
      ZipFileReader.readZipFile(zis)
    }
  }

  it should "throw IllegalArgumentException when a zip entry contains nested path traversal" in {
    val entries = Seq(
      "build/../../secret.txt" -> "malicious".getBytes("UTF-8")
    )
    val zis = createZipStream(entries)

    an [IllegalArgumentException] should be thrownBy {
      ZipFileReader.readZipFile(zis)
    }
  }
}
