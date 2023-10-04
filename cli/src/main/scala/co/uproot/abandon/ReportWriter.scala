package co.uproot.abandon

import java.io.FileWriter
import java.nio.file.{Files, Paths}


final class ReportWriter(settings: Settings, outFiles: Seq[String]) {
  val writesToScreen = settings.writesToScreen(outFiles)
  val filePaths = settings.getConfigRelativePaths(outFiles)
  val fileWriters = filePaths.map(pathStr =>  {
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

  def startCodeBlock() = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def endCodeBlock() = {
    fileWriters foreach { fileWriter =>
      fileWriter.write("```\n")
    }
  }

  def printHeading(head: String) = {
    fileWriters foreach { fileWriter =>
      fileWriter.write(head + "\n\n")
    }

    if (writesToScreen) {
      Console.println(head + "\n" + ("─" * head.length) + "\n")
    }
  }

  def println(s: String*) = {
    fileWriters foreach { fileWriter =>
      s.foreach(str => fileWriter.write(str))
      fileWriter.write('\n')
    }

    if (writesToScreen) {
      s.foreach(print(_))
      Console.println()
    }
  }

  def printXml(x: xml.Node) = {
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

  def close = {
    fileWriters foreach { fileWriter =>
      fileWriter.close
    }
    Console.flush
  }
}
