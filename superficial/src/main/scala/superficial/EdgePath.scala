package superficial

sealed trait EdgePath{
    val initial: Vertex
    val terminal: Vertex
}

object EdgePath{
    final case class Constant(initial: Vertex) extends EdgePath{
        val terminal: Vertex = initial
    }

    final case class Append(init: EdgePath, last: Edge) extends EdgePath{
        val initial: Vertex = init.initial
        val terminal: Vertex = last.terminal
        require(init.terminal == last.initial, 
        s"the terminal vertex ${init.terminal} of the initial segment $init is not the initial vertex ${last.initial} of the final edge $last.")
    }

    def length(path: EdgePath) : Int =
        path match {
            case Append(init, last) => length(init) + 1
            case Constant(initial) => 0
        }

    def apply(v: Vector[Edge]) : EdgePath = ??? // implement this

    def boundary(poly: Polygon) : EdgePath = ??? // implement this

    def isReduced(path: EdgePath) : Boolean = ??? // implement
}