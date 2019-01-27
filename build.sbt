val scalaV = "2.12.8"

val ammV = "1.6.0"

scalaVersion in ThisBuild := scalaV

lazy val freegroups = project.settings(
  name := "freegroups",
  libraryDependencies ++= Seq(
    // "com.lihaoyi" %% "ammonite-ops" % ammV,
    "io.monix"      %% "monix"         % "3.0.0-RC2"
  )
)
