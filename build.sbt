lazy val commonSettings = Seq(
  organization := "in.ac.iisc",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "superficial"
  )
