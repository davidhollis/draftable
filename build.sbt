name := """draftable"""
organization := "computer.hollis"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.3"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "computer.hollis.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "computer.hollis.binders._"

scalacOptions ++= Seq(
  "-deprecation",
  "-Xlint:unused",
  "-Wdead-code",
)
scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Xlint:unused")) }
scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
