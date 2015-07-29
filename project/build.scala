import sbt._
import Keys._

object HelloBuild extends Build {
    lazy val root = Project(id = "abandon", base = file(".")) dependsOn(cli, gui) aggregate(base)

    lazy val base:Project = Project(id = "base", base = file("base"))

    lazy val cli = Project(id = "cli", base = file("cli")) dependsOn(base)

    lazy val gui = Project(id = "gui", base = file("gui")) dependsOn(base)
}
