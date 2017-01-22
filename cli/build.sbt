import Dependencies._

libraryDependencies += scalatest % "test"
libraryDependencies += dirSuite % "test"

fork in Test := true
