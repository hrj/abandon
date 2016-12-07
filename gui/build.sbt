import Dependencies._

libraryDependencies += scalaFX

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
    cp filter {_.data.getName == "jfxrt.jar"}
}

fork in run := true
