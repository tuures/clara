ThisBuild / scalaVersion := "2.13.11"
ThisBuild / organization := "com.example"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val main = (project in file("."))
  .settings(
    name := "Clara",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.0.2",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.16" % Test
    ),
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      // "-Xlint",
      "-Wnumeric-widen",
      "-Wunused",
      "-Wvalue-discard",
    )
  )
