lazy val commonSettings = Seq(
  organization := "in.ac.iisc",
  version := "0.1.0",
  scalaVersion := "2.12.4"
)



lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "superficial",
    libraryDependencies += "io.monix" %% "monix" % "3.0.0-M2",
    libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.3" cross CrossVersion.full
  )
