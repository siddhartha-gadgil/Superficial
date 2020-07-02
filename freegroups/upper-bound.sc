import $ivy.`org.scalanlp::breeze:1.0`
import breeze.linalg._
import breeze.stats.distributions._
def l(k: Int, n: Int) = math.abs(2* k -n)
def L(n: Int) = {
    val bin = Binomial(n, 0.5)
    (0 to n).map(j => bin.probabilityOf(j) * l(j, n)).sum}
val runAverages = LazyList.from(1).map(j => j -> L(j))
def bound(n: Int) = runAverages.take(n).map{case (j, p) => 0.5 * math.pow(0.5, j - 1) * p}.sum / 4