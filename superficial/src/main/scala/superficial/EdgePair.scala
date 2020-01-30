package superficial

class EdgePair(initial: Vertex, terminal: Vertex) { pair =>
  case object Positive extends OrientedEdge {
    val initial = pair.initial
    val terminal = pair.terminal

    val positivelyOriented: Boolean = true

    lazy val flip: OrientedEdge = Negative

    override def toString = s"$pair.positive"
  }

  case object Negative extends OrientedEdge {
    val initial = pair.terminal
    val terminal = pair.initial

    val positivelyOriented: Boolean = false

    lazy val flip: OrientedEdge = Positive

    override def toString = s"$pair.negative"
  }
}