ThisBuild / scalaVersion := "2.13.18"
ThisBuild / organization := "com.example"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val main = (project in file("."))
  .settings(
    name := "Clara",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.19" % Test
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
