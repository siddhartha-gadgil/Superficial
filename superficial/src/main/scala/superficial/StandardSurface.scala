package superficial

class StandardSurface(genus: Int) extends TwoComplex{surface =>
    lazy val faces: Set[Polygon] = Set(face)
    lazy val edges: Set[Edge] = face.edges
    lazy val vertices: Set[Vertex] = Set(vertex)

    lazy val face : Polygon = Polygon(boundary)

    import StandardSurface._

    lazy val boundary : Vector[Edge] = (0 until genus).flatMap(
        k => Vector(A(k, surface, true), B(k, surface, true), A(k, surface, false), B(k, surface, false))
    ).toVector

    val vertex = new Vertex

    
}

object StandardSurface{
    case class A(index: Int, surf: StandardSurface, positive: Boolean) extends Edge{
        def flip: Edge = A(index, surf, !positive)
        lazy val terminal: Vertex = surf.vertex
        lazy val initial: Vertex = surf.vertex
    }
    case class B(index: Int, surf: StandardSurface, positive: Boolean) extends Edge{
        def flip: Edge = B(index, surf, !positive)
        lazy val terminal: Vertex = surf.vertex
        lazy val initial: Vertex = surf.vertex
    }
   
}