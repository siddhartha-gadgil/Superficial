
package ammonite
package $file.freegroups
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.repl.tools.{
  desugar,
  source
}
import _root_.ammonite.main.Router.{
  doc,
  main
}
import _root_.ammonite.repl.tools.Util.{
  pathScoptRead
}


object `upper-bound`{
/*<script>*/import $ivy.$                         
import breeze.linalg._
import breeze.stats.distributions._
def l(k: Int, n: Int) = math.abs(2* k -n)
def L(n: Int) = {
    val bin = Binomial(n, 0.5)
    (0 to n).map(j => bin.probabilityOf(j) * l(j, n)).sum}
val runAverages = LazyList.from(1).map(j => j -> L(j))
def bound(n: Int) = runAverages.take(n).map{case (j, p) => 0.5 * math.pow(0.5, j - 1) * p}.sum / 4/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "upper$minusbound"
  /*</generated>*/
}
