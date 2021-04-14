package smtsolve

object Collinear {
  def apply(
      x1: RealExpr,
      y1: RealExpr,
      x2: RealExpr,
      y2: RealExpr,
      x3: RealExpr,
      y3: RealExpr
  ): BoolExpr =
    (y2 - y1) * (x3 - x1) =:=
      (y3 - y1) * (x2 - x1)

  val x = RealExpr("x")
  val y = RealExpr("y")

  val oppositeCollinear: BoolExpr = Collinear(x, y, RealExpr(0.0), RealExpr(0.0), -x, -y)

  val oppositeDoc: SMTDoc = SMTDoc(Vector(x, y), Vector(!oppositeCollinear)).addCheck

  def oppositeCheck() = 
      oppositeDoc.z3Run().out      
}
