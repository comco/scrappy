name := "scrappy"

version := "0.1"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

instrumentSettings

CoverallsPlugin.coverallsSettings
