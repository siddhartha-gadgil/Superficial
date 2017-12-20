lazy val commonSettings = Seq(
  organization := "in.ac.iisc",
  version := "0.1.0",
  scalaVersion := "2.12.4"
)

libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.3" % "test" cross CrossVersion.full

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "superficial",
    libraryDependencies += "io.monix" %% "monix" % "3.0.0-M2",
    sourceGenerators in Test += Def.task {
        val file = (sourceManaged in Test).value / "amm.scala"
        val initCommands =
          """import superficial._, freegroups._"""
        IO.write(
          file,
          s"""object amm extends App { ammonite.Main("$initCommands").run() }""")
        Seq(file)
    }.taskValue
  )
