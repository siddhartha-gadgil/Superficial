package superficial

import PantsSurface._

case class Z3(n: Int) {
  require(0 <= n && n <= 2, s"$n not valid mod 3 representative")
  def next = Z3((n + 1) % 3)
  def prev = Z3((n + 2) % 3)

  def others: Set[Z3] = Z3.enum - this
}

object Z3 {
  val enum: Set[Z3] = Set(0, 1, 2).map(Z3(_))
}

/**
  * index for boundary of pants, may be in the curve system or the boundary of the surface
  * @param pants the index of the pair of pants
  * @param direction the prong of the pair of pants
  */
case class PantsBoundary(pants: Index, direction: Z3)

case class BoundaryVertex(pb: PantsBoundary, first: Boolean) extends Vertex

case class BoundaryEdge(pb: PantsBoundary,
                        top: Boolean,
                        positiveOriented: Boolean)
    extends Edge {
  lazy val flip = BoundaryEdge(pb, top, !positiveOriented)
  lazy val head : BoundaryVertex =
     BoundaryVertex(pb, positiveOriented)


  lazy val tail: BoundaryVertex =
      BoundaryVertex(pb, !positiveOriented)

}

abstract class Hexagon extends Polygon(6)

case class PantsSeam(pants: Index, head: Vertex, tail: Vertex) extends Edge {
  lazy val flip = PantsSeam(pants, tail, head)
}

case class Curve(left: PantsBoundary, right: PantsBoundary)

case class CurveVertex(curve: Curve, first: Boolean) extends Vertex

case class CurveEdge(curve: Curve, top: Boolean, positivelyOriented: Boolean)
    extends Edge {
  lazy val flip = CurveEdge(curve, top, !positivelyOriented)

  lazy val head : Vertex =
    CurveVertex(curve, positivelyOriented ^ top)

  lazy val tail: Vertex =
    CurveVertex(curve, !(positivelyOriented ^ top))
}

case class PantsHexagon(pants: Index, top: Boolean, cs: Set[Curve])
    extends Hexagon {
  val vertices: Set[Vertex] =
    for {
      direction <- Z3.enum
      first <- Set(true, false)
    } yield vertex(PantsBoundary(pants, direction), first, cs)

  val seams: Set[Edge] =
    for {
      direction <- Z3.enum
    } yield seam(pants, direction, cs)

  val boundaryEdges: Set[Edge] =
    for {
      direction <- Z3.enum
      top <- Set(true, false)
      positivelyOriented <- Set(true, false)
    } yield edge(PantsBoundary(pants, direction), top, positivelyOriented, cs)

  val edges: Set[Edge] = seams union boundaryEdges
}

case class PantsSurface(numPants: Index, cs: Set[Curve]) extends PureTwoComplex {
  val faces : Set[Polygon] =
    for {
      pants: Index <- (0 until numPants).toSet
      top <- Set(true, false)
    } yield PantsHexagon(pants, top, cs)
}

object PantsSurface {
  type Index = Int

  def getCurve(pb: PantsBoundary, cs: Set[Curve]): Option[Curve] =
    cs.find(
      (c) => c.left == pb || c.right == pb
    )

  def edge(pb: PantsBoundary,
           top: Boolean,
           positvelyOriented: Boolean,
           cs: Set[Curve]): Edge =
    getCurve(pb, cs)
      .map(
        (curve) => CurveEdge(curve, top, positvelyOriented)
      )
      .getOrElse(BoundaryEdge(pb, top, positvelyOriented))

  def vertex(pb: PantsBoundary, first: Boolean, cs: Set[Curve]): Vertex =
    getCurve(pb, cs)
      .map(
        (curve) => CurveVertex(curve, first)
      )
      .getOrElse(BoundaryVertex(pb, first))

  def seam(pants: Index, direction: Z3, cs: Set[Curve]): PantsSeam = {
    val head = vertex(PantsBoundary(pants, direction), first = true, cs)
    val tail = vertex(PantsBoundary(pants, direction.next), first = false, cs)
    PantsSeam(pants, head, tail)
  }
}
