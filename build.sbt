val scalaV = "2.13.4"

ThisBuild / organization := "in.ac.iisc"
ThisBuild / version := "0.1.1"

ThisBuild / githubOwner := "siddhartha-gadgil"
ThisBuild / githubRepository := "Superficial"

ThisBuild / githubTokenSource := Some(TokenSource.GitConfig("github.token"))

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

scalaVersion in ThisBuild := scalaV

lazy val freegroups = project.settings(
  name := "freegroups",
  libraryDependencies ++= Seq(
    "io.monix" %% "monix" % "3.3.0",
    "org.typelevel" %% "spire" % "0.17.0"
  )
)

lazy val superficial = project.settings(
  name := "superficial",
  ThisBuild / organization := "in.ac.iisc",
  ThisBuild / version := "0.1.1",
  ThisBuild / githubOwner := "siddhartha-gadgil",
  ThisBuild / githubRepository := "Superficial",
  ThisBuild / githubTokenSource := Some(TokenSource.GitConfig("github.token")),
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.0",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test",
  libraryDependencies += "org.creativescala" %% "doodle" % "0.9.21",
  libraryDependencies += "com.lihaoyi" %%% "upickle" % "1.4.0",
  libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8",
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val polymath = project.settings(
  name := "polymath",
  libraryDependencies ++= Seq(
    // "com.lihaoyi" %% "ammonite-ops" % ammV,
    "io.monix" %% "monix" % "3.3.0",
    "org.typelevel" %% "spire" % "0.17.0",
    "com.lihaoyi" %% "pprint" % "0.6.0"
  )
)
