import scala.concurrent.duration._

lazy val commonSettings = Seq(
  // don't define "name" here, because it will cause
  // circular dependencies with sub-projects

  organization := "in.co.uproot",
  version := "0.8.0",
  scalaVersion := "3.8.4",
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

lazy val mutationTestSettings = Seq(
  strykerReporters := Seq("console", "html", "json"),
  strykerOpenReport := false,
  strykerConcurrency := 1,
  strykerTimeout := 30.seconds,
  strykerTimeoutFactor := 2.0,
  strykerExcludedMutations := Seq("MethodExpression", "StringLiteral"),
  // Focused evaluation and report tests keep the full-base score above 45%.
  strykerThresholdsLow := 56,
  strykerThresholdsBreak := 55
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
  settings(mutationTestSettings: _*).
  settings(
    name := "abandon-base",
    strykerMutate := Seq(
      "src/main/scala/**/*.scala",
      "!src/main/scala/**/FileWatcher.scala"
    ),
    // These asynchronous/integration-style suites do not terminate cleanly in
    // Stryker4s's reusable test runner. Their production code is still covered
    // by the remaining processor and parser suites where applicable.
    strykerTestFilter := Seq(
      "co.uproot.abandon.AstTest",
      "co.uproot.abandon.ConfigTest",
      "co.uproot.abandon.DateConstraintTest",
      "co.uproot.abandon.EvaluationContextTest",
      "co.uproot.abandon.GlobTest",
      "co.uproot.abandon.GroupByTest",
      "co.uproot.abandon.HelperTest",
      "co.uproot.abandon.ParserTest",
      "co.uproot.abandon.ProcessorTest",
      "co.uproot.abandon.ReportsTest"
    ),
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
  settings(mutationTestSettings: _*).
  settings(
    name := "abandon-cli",
    // CLIApp depends on the generated frontend bundle. Keep the baseline
    // mutation run reproducible on a clean checkout by targeting independently
    // tested Scala code.
    strykerMutate := Seq("src/main/scala/**/JsonUtils.scala"),
    strykerTestFilter := Seq("co.uproot.abandon.web.JsonUtilsTest"),
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

addCommandAlias("mutationTest", ";base/stryker;cli/stryker")
