package superficial

class EdgePair(val initial: Vertex, val terminal: Vertex) { pair =>
  lazy val  Positive = EdgePair.Oriented(pair, true)

  lazy val Negative = EdgePair.Oriented(pair, false)
}

object EdgePair{
  case class Oriented(pair: EdgePair, positivelyOriented: Boolean) extends OrientedEdge{
    lazy val flip: OrientedEdge = Oriented(pair, !positivelyOriented)
    lazy val terminal: Vertex = if (positivelyOriented) pair.terminal else pair.initial
    lazy val initial: Vertex = if (positivelyOriented) pair.initial else pair.terminal
  }
}