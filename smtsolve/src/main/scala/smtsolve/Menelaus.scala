package smtsolve

/**
  * This is translating and replicating work done by Anand Tadipatri
  */
object Menelaus {
  val ax = RealExpr("x_a")
  val ay = RealExpr("y_a")
  val bx = RealExpr(0)
  val by = RealExpr(0)
  val cx = RealExpr(1)
  val cy = RealExpr(0)

  val r = RealExpr("r")
  val s = RealExpr("s")
  val t = RealExpr("t")

  def cut(
      x1: RealExpr,
      y1: RealExpr,
      x2: RealExpr,
      y2: RealExpr,
      l: RealExpr
  ): (RealExpr, RealExpr) =
    (x1 + l * (x2 - x1), y1 + l * (y2 - y1))

  val (dx, dy) = cut(ax, ay, bx, by, r)
  val (ex, ey) = cut(bx, by, cx, cy, s)
  val (fx, fy) = cut(cx, cy, ax, ay, t)

  def d(x1: RealExpr, y1: RealExpr, x2: RealExpr, y2: RealExpr) =
    (x2 - x1) ** 2.0 + (y2 - y1) ** 2.0

  val distEq =
    d(ax, ay, dx, dy) * d(bx, by, ex, ey) * d(cx, cy, fx, fy) =:=
      d(dx, dy, bx, by) * d(ex, ey, cx, cy) * d(fx, fy, ax, ay)

  def inBounds(l: RealExpr) = (l > 0) & (l < 1)

  val oneNotInBounds =
    (!inBounds(r) & inBounds(s) & inBounds(t)) |
      (inBounds(r) & !inBounds(s) & inBounds(t)) |
      (inBounds(r) & inBounds(s) & !inBounds(t))

  val threeNotInBounds = (!inBounds(r) & !inBounds(s) & !inBounds(t))

  val variables = Vector(ax, ay, r, s, t)

  val forwardDoc = SMTDoc(
    variables,
    Vector(ay > 0.0, Collinear(dx, dy, ex, ey, fx, fy), !distEq),
    logicOpt = None,
    nameOpt = Some("menelaus.smt2")
  )
}
