package smtwrap

object Pappus {
  val zero = RealExpr(0.0)

  val ax = RealExpr(1)
  val u = RealExpr("u")
  val bx = ax + u
  val v = RealExpr("v")
  val cx = bx + v

  val ay = zero
  val by = zero
  val cy = zero

  val Ax = RealExpr("Ax")
  val Ay = RealExpr("Ay")
  val U = RealExpr("U")
  val Bx = Ax * (U + 1.0)
  val By = Ay * (U + 1.0)
  val V = RealExpr("V")
  val Cx = Ax * (U + V + 1.0)
  val Cy = Ay * (U + V + 1.0)

  val Px = RealExpr("Px")
  val Py = RealExpr("Py")
  val Qx = RealExpr("Qx")
  val Qy = RealExpr("Qy")
  val Rx = RealExpr("Rx")
  val Ry = RealExpr("Ry")

  val intersectionEquations: Vector[BoolExpr] =
    Vector(
      Collinear(ax, ay, Px, Py, Bx, By),
      Collinear(Ax, Ay, Px, Py, bx, by),
      Collinear(ax, ay, Qx, Qy, Cx, Cy),
      Collinear(Ax, Ay, Qx, Qy, cx, cy),
      Collinear(bx, by, Rx, Ry, Cx, Cy),
      Collinear(Bx, By, Rx, Ry, cx, cy)
    )

  val contra = !Collinear(Px, Py, Qx, Qy, Rx, Ry)

  val positiveEquations: Vector[BoolExpr] =
    Vector(u, v, Ay, U, V).map(exp => exp >= 0.0)

  val equations = (intersectionEquations ++ positiveEquations) :+ contra

  val variables = Vector(u, v, Ax, Ay, U, V, Px, Py, Qx, Qy, Rx, Ry)

  val rnd = new scala.util.Random()

  def setRandomPositive(x: RealExpr): BoolExpr =
    (x =:= math.abs(rnd.nextGaussian()))

  def smt2(fixed: Vector[String] = Vector()): SMTDoc = {
    val fixEquations = fixed.map(s => setRandomPositive(RealExpr(s)))
    SMTDoc(variables, equations ++ fixEquations).addCheck
  }
}
