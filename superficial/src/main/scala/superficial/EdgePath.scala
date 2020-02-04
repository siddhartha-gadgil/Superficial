package superficial

import NonPosQuad._

sealed trait EdgePath{ edgePath =>
    import EdgePath._
    val initial: Vertex
    val terminal: Vertex

    /**
    * Concatenation for EdgePaths
    */
    def ++(path:EdgePath): EdgePath = {
        path match{
            case Constant(initial) => edgePath
            case Append(Constant(init), e) => Append(edgePath, e)
            case Append(Append(ep, e1), e2) => Append((edgePath ++ Append(ep,e1)), e2)
        }
    }

    /**
      * More natural notation for appending an edge
      *
      * @param e
      * @return
      */
    def +(e: Edge): EdgePath = Append(edgePath, e)
}

object EdgePath{

    /**
     * Constructor for a constant EdgePath
     */
    final case class Constant(initial: Vertex) extends EdgePath{
        val terminal: Vertex = initial
    }

    /**
      * Inductive constructor for EdgePaths
      *
      * @param init
      * @param last
      */
    final case class Append(init: EdgePath, last: Edge) extends EdgePath{
        val initial: Vertex = init.initial
        val terminal: Vertex = last.terminal
        require(init.terminal == last.initial, 
        s"the terminal vertex ${init.terminal} of the initial segment $init is not the initial vertex ${last.initial} of the final edge $last.")
    }

    /**
      * Finds the length of an EdgePath
      *
      * @param path
      * @return length of path
      */
    def length(path: EdgePath) : Int = {
        path match {
            case Append(init, last) => length(init) + 1
            case Constant(initial) => 0
        }
    }
     
    /**
      * Constructing EdgePaths from vectors of edges
      *
      * @param v
      * @return EdgePath
      */
    def apply(v: Vector[Edge]) : EdgePath = {
        assert(v.nonEmpty)
        v match {
            case y+:(z+:ys) => Append(apply(v.init), v.last)
            case Vector(y) => Append(Constant(y.initial), y)
        }
    }

    /**
      * Converts the boundary of a polygon to an EdgePath
      *
      * @param poly
      * @return EdgePath
      */
    def boundary(poly: Polygon) : EdgePath = {
        EdgePath(poly.boundary)
    }

    /**
      * Checks if an EdgePath is reduced
      *
      * @param path
      * @return
      */
    def isReduced(path: EdgePath) : Boolean = {
        path match {
            case Append(Append(ep, e1),e2) => isReduced(Append(ep,e1)) && (e1 != e2.flip)
            case Append(Constant(initial), e) => true
            case Constant(initial) => true
        }
    }

    /**
      * Gives a vector of edges corresponding to an EdgePath
      *
      * @param path
      * @return
      */
    def edgeVectors(path: EdgePath): Vector[Edge] = {
        path match{
            case Constant(initial) => Vector[Edge]()
            case Append(init, last) => edgeVectors(init) :+ last
        }
    }

    /**
      * Gives a vector of integers corresponding to turns in an EdgePath
      *
      * @param path
      * @return
      */
    def turnPath(path: EdgePath, nonPosQuad: NonPosQuad): Vector[Int] = {
        val v = edgeVectors(path)
        v.zip(v.tail :+ v.head).map(v=> nonPosQuad.turnIndex(v._1,v._2))
    }

    /**
      * Gives an EdgePath from an initial edge and a vector of turn directions
      *
      * @param v
      * @param e
      * @return
      */
    def turnPathToEdgePath(v: Vector[Int], e: Edge, nonPosQuad: NonPosQuad): EdgePath = {
        def accumTurnPathToEdgePath(vect: Vector[Int], edge: Edge, accum: EdgePath): EdgePath = {
            if (! vect.isEmpty) {
                val e = nonPosQuad.turnEdge(edge, vect.head)
                accumTurnPathToEdgePath(vect.tail, e, Append(accum, e))
            }
            else accum
        }
        accumTurnPathToEdgePath(v, e, Append(Constant(e.initial), e))
    }

    /**
      * Finds indices of the beginning and end of left brackets in an EdgePath
      *
      * @param v
      * @param e
      * @return
      */
    def findLeftBracketsTurnPath(v: Vector[Int]): Vector[(Int,Int)] = {
        def checkSegment(turnVect: Vector[Int], indices: (Int,Int), accum: Vector[(Int,Int)]): Vector[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == 2) checkSegment(turnVect.tail, (indices._1, indices._2+1), accum)
                else if (turnVect.head == 1) checkSegment(turnVect.tail, (indices._2, indices._2+1), accum :+ indices)
                else traversePath(turnVect.tail, indices._2+1, accum)
            }
            else accum 
        }

        def traversePath(turnVect: Vector[Int], index: Int, accum: Vector[(Int,Int)]): Vector[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == 1) checkSegment(turnVect.tail,(index, index+1), accum)
                else traversePath(turnVect.tail, index, accum)
            }
            else accum
        }
        traversePath(v, 0, Vector[(Int,Int)]())

    }

    /**
      * Finds indices of the beginning and end of left brackets in an EdgePath
      *
      * @param v
      * @return
      */
    def findRightBracketsTurnPath(v: Vector[Int]): Vector[(Int,Int)] = {
        def checkSegment(turnVect: Vector[Int], indices: (Int,Int), accum: Vector[(Int, Int)]): Vector[(Int, Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == -2) checkSegment(turnVect.tail, (indices._1, indices._2+1), accum)
                else if (turnVect.head == -1) checkSegment(turnVect.tail, (indices._2, indices._2+1), accum :+ indices)
                else traversePath(turnVect.tail, indices._2+1, accum)
            }
            else accum 
        }

        def traversePath(turnVect: Vector[Int], index: Int, accum: Vector[(Int,Int)]): Vector[(Int, Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == -1) checkSegment(turnVect.tail,(index, index+1), accum)
                else traversePath(turnVect.tail, index, accum)
            }
            else accum
        }
        traversePath(v, 0, Vector[(Int,Int)]())
        
    }

}