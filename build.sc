import mill._
import mill.scalalib._, mill.scalajslib._
import ammonite.ops._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.8"
}

object superficial extends CommonModule with ScalaJSModule with SbtModule{
  def scalaJSVersion = "0.6.25"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-xml:1.1.1"
  )

  def bin() : define.Command[PathRef] = T.command {
    def ass: PathRef = assembly()
    def name: String = artifactName()
    cp.over(ass.path, pwd/ "superficial" / "notes" / "superficial.jar")
    ass
  }

}

object freegroups extends CommonModule with SbtModule {
  def ivyDeps = Agg(
    ivy"io.monix::monix:3.0.0-RC2",
    ivy"org.typelevel::spire:0.16.0"
  )

  override def mainClass = Some("freegroups.ProofScript")
}
