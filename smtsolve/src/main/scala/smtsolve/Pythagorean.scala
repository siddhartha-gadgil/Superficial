package smtsolve

import Pythagorean._

case class Pythagorean(n: Int, verbose: Boolean = false) {
  val vars = Vector.tabulate(n)(j => BoolExpr(s"x_${j + 1}"))

  lazy val triples =
    (for {
      i <- 1 to n
      j <- 1 to n
      k = math.sqrt((i * i) + (j * j)).toInt
      if (k <= n) && ((i * i) + (j * j) == k * k)
    } yield (i, j, k)).toVector

  lazy val equations = triples.map {
    case (i, j, k) => notMonochrome(vars(i - 1), vars(j - 1), vars(k - 1))
  }

  lazy val smtDoc = SMTDoc(
    vars,
    equations,
    init =
      if (verbose) Vector(SMTCommand("(set-option :verbosity 2)")) else Vector()
  )

  def getSolution(commands: Vector[String]) =
    smtDoc.seekValues(commands).map(m => Vector.tabulate(n)(j => m(vars(j))))

  lazy val solution = getSolution(SMTDoc.z3Interactive)

}

object Pythagorean {
  def notMonochrome(x: BoolExpr, y: BoolExpr, z: BoolExpr): BoolExpr =
    (x | y | z) & (!(x & y & z))
}
