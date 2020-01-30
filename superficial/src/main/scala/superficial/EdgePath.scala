package superficial

sealed trait EdgePath{ edgePath =>
    import EdgePath._
    val initial: Vertex
    val terminal: Vertex

    def ++(path:EdgePath): EdgePath = {
        path match{
            case Constant(initial) => edgePath
            case Append(Constant(init), e) => Append(edgePath, e)
            case Append(Append(ep, e1), e2) => Append((edgePath ++ Append(ep,e1)), e2)
        }
    }
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

    def length(path: EdgePath) : Int = {
        path match {
            case Append(init, last) => length(init) + 1
            case Constant(initial) => 0
        }
    }
     
    def apply(v: Vector[Edge]) : EdgePath = {
        assert(v.nonEmpty)
        v match {
            case y+:(z+:ys) => Append(apply(v.init), v.last)
            case Vector(y) => Append(Constant(y.initial), y)
        }
    }

    def boundary(poly: Polygon) : EdgePath = {
        EdgePath(poly.boundary)
    }

    def isReduced(path: EdgePath) : Boolean = {
        path match {
            case Append(Append(ep, e1),e2) => isReduced(Append(ep,e1)) && (e1 != e2.flip)
            case Append(Constant(initial), e) => true
            case Constant(initial) => true
        }
    }
}