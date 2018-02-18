import mill._
import mill.scalalib._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.4"
}

object superficial extends CommonModule

object freegroups extends CommonModule
