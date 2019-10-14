val scalaV = "2.12.8"

val ammV = "1.6.0"

scalaVersion in ThisBuild := scalaV

lazy val freegroups = project.settings(
  name := "freegroups",
  libraryDependencies ++= Seq(
    // "com.lihaoyi" %% "ammonite-ops" % ammV,
    "io.monix"      %% "monix"         % "3.0.0-RC2",
    "org.typelevel" %% "spire" % "0.16.0"
  )
)

lazy val superficial = project.settings(
  name := "superficial",
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

)

lazy val polymath = project.settings(
  name := "polymath",
  libraryDependencies ++= Seq(
    // "com.lihaoyi" %% "ammonite-ops" % ammV,
    "io.monix"      %% "monix"         % "3.0.0-RC2",
    "org.typelevel" %% "spire" % "0.16.0",
    "com.lihaoyi" %% "pprint" % "0.5.2"
  )
)