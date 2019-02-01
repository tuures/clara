ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "com.example"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val main = (project in file("."))
  .settings(
    name := "Clara",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "0.4.2",
      "com.github.nikita-volkov" % "sext" % "0.2.4",
      "ai.x" %% "safe" % "0.1.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    ),
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused"
    )
  )
