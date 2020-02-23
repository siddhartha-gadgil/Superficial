package superficial

class EdgePair(val initial: Vertex, val terminal: Vertex) { pair =>
  lazy val  Positive = EdgePair.Oriented(pair, true)

  lazy val Negative = EdgePair.Oriented(pair, false)
}

object EdgePair{
  case class Oriented(pair: EdgePair, positive: Boolean) extends Edge{
    lazy val flip: Edge = Oriented(pair, !positive)
    lazy val terminal: Vertex = if (positive) pair.terminal else pair.initial
    lazy val initial: Vertex = if (positive) pair.initial else pair.terminal
  }
}