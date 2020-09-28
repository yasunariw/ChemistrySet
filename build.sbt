name := "ChemistrySet"

version := "0.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

//scalacOptions += "-optimize"

//scalacOptions += "-Yinline"

//scalacOptions += "-Ydebug"

//scalacOptions += "-Ylog:inliner"

parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

fork := true

javaOptions += "-server"

javaOptions += "-XX:+DoEscapeAnalysis"

javaOptions += "-Xmx2048M"