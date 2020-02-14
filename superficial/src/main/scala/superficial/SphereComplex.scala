package superficial

object SphereComplex {
  case object X extends Vertex
  case object Y extends Vertex
  case object Z extends Vertex

  case object A extends EdgePair(X, Y)
  case object B extends EdgePair(Y, Z)
  case object C extends EdgePair(Z, X)

  val upper = Polygon(Vector(A.Positive, B.Positive, C.Positive))
  val lower = Polygon(Vector(C.Negative, B.Negative, A.Negative))

  val doubleTriangle: TwoComplex = TwoComplex.pure(upper, lower)

  val doubleBigon: TwoComplex = doubleTriangle.collapseEdge(A.Positive)._1
  val doubleMonogon = TwoComplex.allCollapsed(doubleTriangle)
}