package smtsolve

final case class UnitQuaternion(
    re: RealExpr,
    iCoeff: RealExpr,
    jCoeff: RealExpr,
    kCoeff: RealExpr
) {
  val components: Vector[RealExpr] = Vector(re, iCoeff, jCoeff, kCoeff)

  def *(that: UnitQuaternion): UnitQuaternion =
    UnitQuaternion(
      re * that.re - iCoeff * that.iCoeff
        - jCoeff * that.jCoeff - kCoeff * that.kCoeff,
      iCoeff * that.re + re * that.iCoeff
        + jCoeff * that.kCoeff - kCoeff * that.jCoeff,
      jCoeff * that.re + re * that.jCoeff
        - iCoeff * that.kCoeff + kCoeff * that.iCoeff,
      kCoeff * that.re + re * that.kCoeff
        - jCoeff * that.iCoeff + iCoeff * that.jCoeff
    )

  val unary_- : UnitQuaternion = UnitQuaternion(-re, -iCoeff, -jCoeff, -kCoeff)

  val normEquation: BoolExpr =
    re * re +
      iCoeff * iCoeff + jCoeff * jCoeff + kCoeff * kCoeff =:= 1.0

  def =:=(that: UnitQuaternion) =
    (re =:= that.re) & (iCoeff =:= that.iCoeff) & (jCoeff =:= that.jCoeff) & (kCoeff =:= that.kCoeff)
}

object UnitQuaternion {
  def inverse(q: UnitQuaternion) =
    UnitQuaternion(q.re, -q.iCoeff, -q.jCoeff, -q.kCoeff)

  def apply(name: String): UnitQuaternion =
    UnitQuaternion(
      RealExpr(name + "_re"),
      RealExpr(name + "_i"),
      RealExpr(name + "_j"),
      RealExpr(name + "_k")
    )

  def apply(re: Double, i: Double, j: Double, k: Double): UnitQuaternion =
    UnitQuaternion(RealExpr(re), RealExpr(i), RealExpr(j), RealExpr(k))

  val one: UnitQuaternion = UnitQuaternion(1.0, 0.0, 0.0, 0.0)

  import freegroups._
  def fromWord(w: Word, prefix: String = ""): UnitQuaternion =
    if (w.ls.isEmpty) one
    else {
      def byIndex(n: Int) =
        UnitQuaternion(('a' + (n - 1)).toChar.toString() + prefix)
      val terms = w.ls.map { j =>
        if (j > 0) byIndex(j) else inverse(byIndex(-j))
      }
      terms.reduce(_ * _)
    }
}
