assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter { _.data.getName == "jfxrt.jar" }
}

// proguardSettings

// ProguardKeys.options in Proguard ++= Seq("-dontwarn", "-ignorewarnings", "-verbose", "-dontoptimize", "-dontobfuscate")

// ProguardKeys.options in Proguard += ProguardOptions.keepMain("co.uproot.abandon.AbandonApp")

lazy val commonSettings = Seq(
  // don't define "name" here, because it will cause
  // circular dependencies with sub-projects

  organization := "in.co.uproot",
  version := "0.5.0",
  scalaVersion := "2.12.8",
  scalacOptions := List("-deprecation", "-opt:_"),
  wartremoverWarnings ++= Warts.allBut(Wart.ToString, Wart.Throw),

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

)

lazy val abandon = (project in file(".")).
  aggregate(base, cli, gui).
  dependsOn(base, cli, gui).
  settings(commonSettings: _*).
  settings(
    name := "abandon",
    fork in run := true
  )

lazy val base = (project in file("base")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(
    name := "abandon-base",
    fork in run := true,
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
    fork in run := true,
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
    fork in run := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "co.uproot.abandon",
    buildInfoObject := "GuiBuildInfo"
  )



concurrentRestrictions in Global := Seq(
  Tags.limit(Tags.Test, 1)
)
