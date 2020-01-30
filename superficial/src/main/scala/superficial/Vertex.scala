package superficial

/**
  * A vertex in a two-complex
  */
class Vertex

object Vertex {
  case class Symbolic(name: String) extends Vertex
}

/**
  * A vertex obtained by identifications
  */
case class QuotientVertex(vertices: Set[Vertex]) extends Vertex

