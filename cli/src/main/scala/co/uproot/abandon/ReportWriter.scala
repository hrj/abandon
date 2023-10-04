package co.uproot.abandon

import java.io.FileWriter
import java.nio.file.{Files, Paths}


final class ReportWriter(settings: Settings, outFiles: Seq[String]) {
  val writesToScreen: Boolean = settings.writesToScreen(outFiles)
  val filePaths: Seq[String] = settings.getConfigRelativePaths(outFiles)
  private val fileWriters = filePaths.map(pathStr =>  {
    val path = Paths.get(pathStr).normalize
    val parentPath = path.getParent
    if (parentPath != null) {
      if (!Files.exists(parentPath)) {
        if (!settings.quiet) {
          Console.println("Creating directory: " + parentPath)
        }
        Files.createDirectories(parentPath)
      }
    }


    try {
      new FileWriter(pathStr)
    } catch {
      case fnf : java.io.FileNotFoundException => throw new SettingsError("Could not write to " + pathStr + ". " + fnf.getMessage)
    }
  })

  def startCodeBlock(): Unit = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def endCodeBlock(): Unit = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def printHeading(head: String): Unit = {
    fileWriters foreach { fileWriter =>
      fileWriter.write(head + "\n\n")
    }

    if (writesToScreen) {
      Console.println(head + "\n" + ("─" * head.length) + "\n")
    }
  }

  def println(s: String*): Unit = {
    fileWriters foreach { fileWriter =>
      s.foreach(str => fileWriter.write(str))
      fileWriter.write('\n')
    }

    if (writesToScreen) {
      s.foreach(print(_))
      Console.println()
    }
  }

  def printXml(x: xml.Node): Unit = {
    val sb = new StringBuilder
    val pp = new xml.PrettyPrinter(120, 2)
    pp.format(x, sb)
    val res = sb.toArray

    fileWriters foreach { fileWriter =>
      fileWriter.write(res)
    }

    if (writesToScreen) {
      Console.println(sb.toString)
    }
  }

  def close(): Unit = {
    fileWriters foreach { fileWriter =>
      fileWriter.close()
    }
    Console.flush
  }
}
