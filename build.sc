import mill._
import mill.scalalib._, mill.scalajslib._
import ammonite.ops._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.4"
}

object superficial extends CommonModule with ScalaJSModule{
  def scalaJSVersion = "0.6.22"

  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.4",
    ivy"com.lihaoyi::scalatags::0.6.7"
  )

  def pack() = T.command {
    def js = fastOpt()
    def targ = pwd / "docs" / artifactName()
    cp.over(js.path, targ / "out.js")
    cp.over(js.path / up / "out.js.map", targ / "out.js.map")
    js
  }
}

object freegroups extends CommonModule {
  def ivyDeps = Agg(
    ivy"io.monix::monix:3.0.0-RC1",
    ivy"com.lihaoyi:::ammonite:1.0.3-21-05b5d32"
  )
}
