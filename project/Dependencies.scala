import sbt._

object Dependencies {
  lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.27"
  lazy val monix = "io.monix" %% "monix" % "3.0.0-RC2"
  lazy val shims ="com.codecommit" %% "shims" % "1.7.0"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
}
