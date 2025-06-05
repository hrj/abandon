lazy val commonSettings = Seq(
  // don't define "name" here, because it will cause
  // circular dependencies with sub-projects

  organization := "in.co.uproot",
  version := "0.8.0",
  scalaVersion := "3.7.1",
  scalacOptions := List("-deprecation"),
  // wartremoverWarnings ++= Warts.allBut(Wart.ToString, Wart.Throw),

  /*
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
  */
)

lazy val abandon = (project in file(".")).
  aggregate(betterFilesCore, dirsuite, base, cli).
  dependsOn(base, cli).
  settings(commonSettings: _*).
  settings(
    name := "abandon",
    run /fork := true,
    nativeImageOptions ++= List("--initialize-at-build-time", "--no-fallback", "-O2", "-H:IncludeResources=\".*/frontend/build.zip$\""),
    nativeImageJvm := "graalvm-java21",
    nativeImageVersion := "21.0.2"
  )
  .enablePlugins(NativeImagePlugin)

lazy val betterFilesCore = (project in file("better-files-core")).
  settings(commonSettings: _*).
  settings(
    name := "better-files",
  )

lazy val dirsuite = (project in file("dirsuite")).
  dependsOn(betterFilesCore).
  settings(commonSettings: _*).
  settings(
    name := "dirsuite",
    libraryDependencies += Dependencies.scalatest
  )

lazy val base = (project in file("base")).
  enablePlugins(BuildInfoPlugin).
  dependsOn(dirsuite).
  settings(commonSettings: _*).
  settings(
    name := "abandon-base",
    run / fork := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "co.uproot.abandon",
    buildInfoObject := "BaseBuildInfo"
  )


lazy val cli = (project in file("cli")).
  enablePlugins(BuildInfoPlugin).
  dependsOn(base).
  settings(commonSettings: _*).
  settings(
    name := "abandon-cli",
    run / fork := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "co.uproot.abandon",
    buildInfoObject := "CliBuildInfo"
  )

lazy val gui = (project in file("gui")).
  enablePlugins(BuildInfoPlugin).
  dependsOn(base).
  settings(commonSettings: _*).
  settings(
    name := "abandon-gui",
    run / fork := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "co.uproot.abandon",
    buildInfoObject := "GuiBuildInfo"
  )

concurrentRestrictions in Global := Seq(
  Tags.limit(Tags.Test, 1)
)
