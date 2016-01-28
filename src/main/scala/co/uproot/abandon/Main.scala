package co.uproot.abandon

import java.net.{URL, URLClassLoader}

object Main extends App {

  if(args.headOption.getOrElse("") equals "-g") {
    ensureJFXIsAvailable()
    AbandonUI.main(args.tail)
  } else {
    CLIMain.runApp(args)
  }

  def ensureJFXIsAvailable() {
    try {
      Class.forName("javafx.event.EventTarget")
    } catch {
      case e:java.lang.ClassNotFoundException =>
        val javaHome = System.getProperty("java.home")
        addSoftwareLibrary(javaHome, "lib", "jfxrt.jar")
      case e: java.lang.NoClassDefFoundError =>
        val javaHome = System.getProperty("java.home")
        addSoftwareLibrary(javaHome, "lib", "jfxrt.jar")
    }
  }

  def addSoftwareLibrary(filePath:String*) = {
      val file = new java.io.File(filePath.mkString(java.io.File.separator))
      val method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL]);
      method.setAccessible(true);
      method.invoke(ClassLoader.getSystemClassLoader(), file.toURI.toURL);
  }
}
