val scalaV = "2.13.1"

ThisBuild / organization := "in.ac.iisc"
ThisBuild / version      := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := scalaV

lazy val freegroups = project.settings(
  name := "freegroups",
  libraryDependencies ++= Seq(
    "io.monix"      %% "monix"         % "3.1.0",
    "org.typelevel" %% "spire" % "0.17.0-M1"
  )
)

lazy val superficial = project.settings(
  name := "superficial",
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

)

lazy val polymath = project.settings(
  name := "polymath",
  libraryDependencies ++= Seq(
    // "com.lihaoyi" %% "ammonite-ops" % ammV,
    "io.monix"      %% "monix"         % "3.1.0",
    "org.typelevel" %% "spire" % "0.17.0-M1",
    "com.lihaoyi" %% "pprint" % "0.5.6"
  )
)