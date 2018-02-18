import mill._
import mill.scalalib._

object superficial extends ScalaModule {
  def scalaVersion = "2.12.4"
}

object freegroups extends ScalaModule {
  def scalaVersion = "2.12.4"
  def ivyDeps=Agg(
    ivy"io.monix::monix:3.0.0-M2"
  )
}
