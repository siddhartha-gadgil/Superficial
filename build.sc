import mill._
import mill.scalalib._, mill.scalajslib._
import ammonite.ops._
import $ivy.`org.eclipse.jgit:org.eclipse.jgit:3.5.0.201409260305-r`

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.13.4"

  def scalacOptions = Seq("-deprecation")
}

def glog = {
  import java.io._
  import org.eclipse.jgit._
  import storage.file._
  val builder = new FileRepositoryBuilder()
  val repo = builder.findGitDir(new File(".")).readEnvironment(). build()
  val git = new api.Git(repo)
  import scala.jdk.CollectionConverters._
  git.log().call().asScala.head
}

object superficial extends CommonModule with SbtModule{
  def scalaJSVersion = "0.6.32"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-xml:1.3.0",
    ivy"com.lihaoyi::fastparse:2.3.0",
    ivy"com.lihaoyi::upickle::1.4.0",
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.0",
    ivy"org.creativescala::doodle:0.9.21"
  )

  def bin() : define.Command[PathRef] = T.command {
    def ass: PathRef = assembly()
    def name: String = artifactName()
    val hashName = s"$name-${glog.abbreviate(10).name}.jar"
    os.copy.over(ass.path, os.pwd/ "CATG2020" / "notebooks" / "bin" / hashName, createFolders = true)
    os.copy.over(ass.path, os.pwd/ "bin" / hashName, createFolders = true)
    val init = 
    s"""import ${"$"}cp.bin.`$hashName`
    |import superficial._
    |""".stripMargin
    println(init)
    os.write.over(os.pwd / "init-superficial.sc", init)
    ass
  }

  def docs() = T.command{
    def jar = docJar()
    os.copy.over(jar.path / up / "javadoc", os.pwd / "CATG2020" / "static" / "scaladocs", createFolders = true)
    jar
  }

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.7")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object freegroups extends CommonModule with SbtModule {
  def ivyDeps = Agg(
    ivy"io.monix::monix:3.3.0",
    ivy"org.typelevel::spire:0.17.0"
  )

  override def mainClass = Some("freegroups.ProofScript")
}

object polymath extends CommonModule with SbtModule {
  def ivyDeps = Agg(
    ivy"io.monix::monix:3.3.0",
    ivy"org.typelevel::spire:0.17.0",
    ivy"com.lihaoyi::pprint::0.6.0"
  )

  override def mainClass = Some("freegroups.ProofScript")

  def bin() : define.Command[PathRef] = T.command {
    def ass: PathRef = assembly()
    def name: String = artifactName()
    cp.over(ass.path, pwd/ "bin" / "polymath.jar")
    ass
  }

}

object smtsolve extends CommonModule with SbtModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.7.4",
    ivy"com.lihaoyi::fastparse:2.2.2"
  )

  def moduleDeps = Seq(freegroups)
}
