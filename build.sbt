ThisBuild / scalaVersion := "2.13.7"
ThisBuild / organization := "com.example"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val main = (project in file("."))
  .settings(
    name := "Clara",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.2.2",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.10" % Test
    ),
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xlint",
      "-Wnumeric-widen",
      "-Wunused",
      "-Wvalue-discard",
    )
  )
