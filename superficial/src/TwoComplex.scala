package superficial

abstract class Polygon(val sides: Int) extends TwoComplex {
  lazy val faces = Set(this)

  val edges: Set[Edge]

  val vertices: Set[Vertex]
}

object Polygon {
  def apply(n: Int): Polygon = new Polygon(n) { self =>
    lazy val edges: Set[Edge] =
      (for (e <- 0 to (sides - 1); ori <- Set(true, false))
        yield PolygonEdge(self, e, ori)).toSet

    lazy val vertices: Set[Vertex] =
      ((0 to (sides - 1)) map (PolygonVertex(self, _))).toSet
  }
}

trait Vertex

trait Edge {
  def flip: Edge

  def head: Vertex

  def tail: Vertex
}

case class PolygonEdge(polygon: Polygon, index: Int, positiveOriented: Boolean)
    extends Edge {
  lazy val flip: Edge = PolygonEdge(polygon, index, !(positiveOriented))

  lazy val (head, tail) =
    if (positiveOriented)
      (PolygonVertex(polygon, (index + 1) % polygon.sides),
       PolygonVertex(polygon, index))
    else
      (PolygonVertex(polygon, index),
       PolygonVertex(polygon, (index + 1) % polygon.sides))

}

case class PolygonVertex(polygon: Polygon, index: Int) extends Vertex

case class QuotientEdge(edges: Set[Edge]) extends Edge {
  def flip = QuotientEdge(edges map (_.flip))

  def head = QuotientVertex(edges map (_.head))

  def tail = QuotientVertex(edges map (_.tail))

}

case class QuotientVertex(vertices: Set[Vertex]) extends Vertex

trait TwoComplex {
  def faces: Set[Polygon]

  def edges: Set[Edge] // these come in pairs, related by flip (reversing orientation

  def vertices: Set[Vertex]
}

trait PureTwoComplex extends TwoComplex {
  val faces: Set[Polygon]

  val edges: Set[Edge] =
    faces.map(_.edges).foldLeft(Set.empty[Edge])(_ union _)

  val vertices: Set[Vertex] =
    faces.map(_.vertices).foldLeft(Set.empty[Vertex])(_ union _)
}

case class NormalArc(initial: Edge, terminal: Edge, face: Polygon) {
  val flip = NormalArc(initial.flip, terminal.flip, face)
}

case class NormalPath(surface: TwoComplex, edges: Vector[NormalArc]) {
  edges.zip(edges.tail).foreach {
    case (e1, e2) =>
      require(e1.terminal == e2.initial,
              s"terminal point of $e1 is not initial point of $e2")
  }

  val isClosed = edges.last.terminal == edges.head.initial
}
