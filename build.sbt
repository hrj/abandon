import AssemblyKeys._ // put this at the top of the file

assemblySettings

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
    cp filter {_.data.getName == "jfxrt.jar"}
}

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontwarn", "-ignorewarnings", "-verbose", "-dontoptimize", "-dontobfuscate")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("co.uproot.abandon.AbandonApp")

name := "abandon"

version := "0.2.0"

scalaVersion in ThisBuild := "2.11.6"

scalacOptions in ThisBuild := List("-deprecation")

fork in run := true
