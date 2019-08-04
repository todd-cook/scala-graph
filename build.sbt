
organization := "com.wordtrellis"
name := "scala-graph"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.13.0"
scalacOptions += "-deprecation"



lazy val scala_graph = (project in file("."))
  .settings(
    name := "scala-graph",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
  )
