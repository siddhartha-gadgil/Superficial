import mill._
import mill.scalalib._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.4"
}

object superficial extends CommonModule

object freegroups extends CommonModule {
  def ivyDeps=Agg(
    ivy"io.monix::monix:3.0.0-M2",
    ivy"com.lihaoyi:::ammonite:1.0.3-21-05b5d32"
  )
}
