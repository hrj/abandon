import AssemblyKeys._ // put this at the top of the file

assemblySettings

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.20-R6"

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
    cp filter {_.data.getName == "jfxrt.jar"}
}

fork in run := true
