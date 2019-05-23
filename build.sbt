import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.lamedh"
ThisBuild / organizationName := "monadhell"

lazy val root = (project in file("."))
  .settings(
    name := "scratch",
    libraryDependencies ++= Seq(
      scalaz,
      monix,
      shims
    )
  )
