package superficial

class Polygon(val sides: Int) extends TwoComplex{
  lazy val faces = Set(this)

  lazy val edges : Set[Edge] =
    (for (e <- 0 to (sides - 1); ori <- Set(true, false)) yield PolygonEdge(this, e, ori)).toSet
    
  lazy val vertices : Set[Vertex] = ((0 to (sides -1)) map (PolygonVertex(this, _))).toSet
}

trait Vertex

trait Edge{
  def flip : Edge

  def head: Vertex

  def tail: Vertex
}

case class PolygonEdge(polygon: Polygon, index: Int, positiveOriented: Boolean) extends Edge{
  lazy val flip : Edge = PolygonEdge(polygon, index, !(positiveOriented))

  lazy val (head, tail)  = 
    if (positiveOriented) (PolygonVertex(polygon, (index + 1) % polygon.sides), PolygonVertex(polygon, index))
    else (PolygonVertex(polygon, index), PolygonVertex(polygon, (index + 1) % polygon.sides))

}

case class PolygonVertex(polygon: Polygon, index: Int) extends Vertex

case class QuotientEdge(edges: Set[Edge]) extends Edge{
  def flip = QuotientEdge(edges map (_.flip))

  def head = QuotientVertex(edges map (_.head))

  def tail = QuotientVertex(edges map (_.tail))

}

case class QuotientVertex(vertices : Set[Vertex]) extends Vertex

trait TwoComplex{
  def faces: Set[Polygon]

  def edges: Set[Edge] // these come in pairs, related by flip (reversing orientation

  def vertices: Set[Vertex]
}
