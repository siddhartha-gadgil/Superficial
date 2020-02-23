package superficial

import fastparse._, SingleLineWhitespace._

class StandardSurface(val genus: Int) extends TwoComplex{surface =>
    lazy val faces: Set[Polygon] = Set(face)
    lazy val edges: Set[Edge] = face.edges
    lazy val vertices: Set[Vertex] = Set(vertex)

    lazy val face : Polygon = Polygon(boundary)

    import StandardSurface._

    lazy val boundary : Vector[Edge] = (1 to genus).flatMap(
        k => Vector(A(k, surface, true), B(k, surface, true), A(k, surface, false), B(k, surface, false))
    ).toVector

    val vertex = new Vertex
    // Parsers

    def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )

    def aEdge[_: P]: P[Edge] = P(("a" | "A")~number).map(n => A(n, this, true))

    def aInvEdge[_: P]: P[Edge] = P(("a" | "A")~number~"!").map(n => A(n, this, false))

    def bEdge[_: P]: P[Edge] = P(("b" | "B")~number).map(n => B(n, this, true))

    def bInvEdge[_: P]: P[Edge] = P(("b" | "B")~number~"!").map(n => B(n, this, false))
    
    def edge[_: P]  : P[Edge] = P(aInvEdge | bInvEdge | aEdge | bEdge)

    def parseEdge(s: String) : Parsed[Edge] = parse(s, edge(_))

    def edgePath[ _ : P] : P[EdgePath] = P(edge.rep).map(s => EdgePath(s.toVector))

    def parsePath(s: String) : Parsed[EdgePath] = parse(s, edgePath(_))

    def getPath(s: String): EdgePath = parsePath(s).get.value

    def getEdge(s: String): Edge = parseEdge(s).get.value
}

object StandardSurface{
    case class A(index: Int, surf: StandardSurface, positive: Boolean) extends Edge{
        assert(0 < index && index <= surf.genus, s"index $index invalid for genus ${surf.genus} surface")
        def flip: Edge = A(index, surf, !positive)
        lazy val terminal: Vertex = surf.vertex
        lazy val initial: Vertex = surf.vertex
    }
    case class B(index: Int, surf: StandardSurface, positive: Boolean) extends Edge{
        assert(0 < index && index <= surf.genus, s"index $index invalid for genus ${surf.genus} surface")
        def flip: Edge = B(index, surf, !positive)
        lazy val terminal: Vertex = surf.vertex
        lazy val initial: Vertex = surf.vertex
    }
   
}