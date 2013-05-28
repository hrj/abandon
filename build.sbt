import AssemblyKeys._ // put this at the top of the file

assemblySettings

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontwarn", "-ignorewarnings", "-verbose", "-dontoptimize", "-dontobfuscate")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("co.uproot.abandon.AbandonApp")

name := "abandon"

scalaVersion in ThisBuild := "2.10.1"

scalacOptions in ThisBuild := List("-feature")

