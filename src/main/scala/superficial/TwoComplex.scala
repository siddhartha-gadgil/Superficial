package superficial

class Polygon(val sides: Int) extends TwoComplex{
  lazy val polygons = Set(this)

  lazy val edges : Set[Edge] =
    (for (e <- 0 to (sides - 1); ori <- Set(true, false)) yield PolygonEdge(this, e, ori)).toSet
}

trait Vertex

trait Edge{
  def flip : Edge

  def head: Vertex

  def tail: Vertex
}

case class PolygonEdge(polygon: Polygon, edge: Int, positiveOriented: Boolean) extends Edge{
  lazy val flip : Edge = PolygonEdge(polygon, edge, !(positiveOriented))

  lazy val tail = PolygonVertex(polygon, edge)

  lazy val head = PolygonVertex(polygon, (edge + 1) % polygon.sides)
}

case class PolygonVertex(polygon: Polygon, vertex: Int) extends Vertex

case class QuotientEdge(edges: Set[Edge]) extends Edge{
  def flip = QuotientEdge(edges map (_.flip))

  def head = QuotientVertex(edges map (_.head))

  def tail = QuotientVertex(edges map (_.tail))

}

case class QuotientVertex(vertices : Set[Vertex]) extends Vertex

trait TwoComplex{
  def polygons: Set[Polygon]
  def edges: Set[Edge]
}
