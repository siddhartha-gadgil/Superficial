package superficial

/**
  * An oriented edge in a two-complex
  */
trait Edge {

  /**
    * the same (undirected) edge with the opposite orientation.
    */
  def flip: Edge

  def terminal: Vertex

  def initial: Vertex

  def checkFlip: Boolean =
    (flip.terminal == initial) && (flip.initial == terminal) && 
    (flip.flip == this) && (flip != this)

  def del: FormalSum[Vertex] =
    FormalSum.reduced(Vector(terminal -> 1, initial -> -1))
}

object Edge {
  case class Symbolic(
      name: String,
      initial: Vertex,
      terminal: Vertex,
      positivelyOriented: Boolean = true
  ) extends OrientedEdge {
    def flip: OrientedEdge =
      Symbolic(name, terminal, initial, !positivelyOriented)
  }

  def symbolic(
      name: String,
      initialName: String,
      terminalName: String,
      positivelyOriented: Boolean = true
  ) =
    Symbolic(
      name,
      Vertex.Symbolic(initialName),
      Vertex.Symbolic(terminalName),
      positivelyOriented
    )
}

trait OrientedEdge extends Edge {
  val positivelyOriented: Boolean

  def flip: OrientedEdge
}

/**
  * An edge obtained by identifications
  */
case class QuotientEdge(edges: Set[Edge]) extends Edge {
  def flip = QuotientEdge(edges map (_.flip))

  def terminal = QuotientVertex(edges map (_.terminal))

  def initial = QuotientVertex(edges map (_.initial))

}