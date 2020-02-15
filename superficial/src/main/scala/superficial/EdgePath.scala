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

    /**
      * Checks whether the EdgePath is in a given Two Complex
      *
      * @param twoComplex
      * @return
      */
    def inTwoComplex(twoComplex: TwoComplex): Boolean = edgeVectors(edgePath).toSet.subsetOf(twoComplex.edges)

    def reverse : (EdgePath) = {
      edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init,last) => Append(Constant(last.terminal), last.flip).++(init.reverse)
      }
    }

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
      * Enumerate all edge-paths of a given length in a complex satisfying a predicate, assumed hereditary,
      * i.e., if an edge-path satisfies a predicate so should sub-paths. Typical use is generating reduced paths.
      *
      * @param complex the two complex in which we consider paths
      * @param length length of paths to generate
      * @param predicate condition to be satisfied
      * @return
      */
    def enumerate(complex: TwoComplex, length: Int, predicate: EdgePath => Boolean = (_) => true) : LazyList[EdgePath] = 
    {
        if (length == 0) complex.vertices.map(Constant(_)).to(LazyList)
        else for {
            init <- enumerate(complex, length - 1, predicate)
            edge <- complex.edges
            if edge.initial == init.terminal
        } yield Append(init, edge)
    }.filter(predicate)

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
      * Gives a vector of integers corresponding to turns in a non-trivial EdgePath
      *
      * @param path
      * @return
      */
    def turnPath(path: EdgePath, nonPosQuad: NonPosQuad): (Edge, Vector[Int]) = {
        require(! edgeVectors(path).isEmpty, s"The edge path $path is empty")
        val v: Vector[Edge] = edgeVectors(path)
        v match{
            case y +: z +: ys => (v.head, v.zip(v.tail :+ v.head).map(v=> nonPosQuad.turnIndex(v._1,v._2)))
            case Vector(y) => (y, Vector[Int]())
        }
        
    }

    /**
      * Gives an EdgePath from an initial edge and a vector of turn directions
      *
      * @param v
      * @param e
      * @return
      */
    def turnPathToEdgePath(edge: Edge, v: Vector[Int], nonPosQuad: NonPosQuad): EdgePath = {
        def accumTurnPathToEdgePath(edge: Edge, vect: Vector[Int], accum: EdgePath): EdgePath = {
            if (! vect.isEmpty) {
                val e = nonPosQuad.turnEdge(edge, vect.head)
                accumTurnPathToEdgePath(e, vect.tail, Append(accum, e))
            }
            else accum
        }
        accumTurnPathToEdgePath(edge, v, Append(Constant(edge.initial), edge))
    }

    /**
      * Finds indices of the beginning and end (the left turns) of the first left bracket in an EdgePath
      *
      * @param v
      * @return
      */
    def findFirstLeftBracketTurnPath(v: Vector[Int]): Option[(Int,Int)] = {
        def checkSegment(turnVect: Vector[Int], indices: (Int,Int)): Option[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == 2) checkSegment(turnVect.tail, (indices._1, indices._2+1))
                else if (turnVect.head == 1) Some(indices)
                else traversePath(turnVect.tail, indices._2+1)
            }
            else None 
        }

        def traversePath(turnVect: Vector[Int], index: Int): Option[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == 1) checkSegment(turnVect.tail,(index, index+1))
                else traversePath(turnVect.tail, index)
            }
            else None
        }
        traversePath(v, 0)

    }

    /**
      * Finds indices of the beginning and end (the right turns) of the first right bracket in an EdgePath
      *
      * @param v
      * @return
      */
    def findFirstRightBracketTurnPath(v: Vector[Int]): Option[(Int,Int)] = {
        def checkSegment(turnVect: Vector[Int], indices: (Int,Int)): Option[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == -2) checkSegment(turnVect.tail, (indices._1, indices._2+1))
                else if (turnVect.head == -1) Some(indices)
                else traversePath(turnVect.tail, indices._2+1)
            }
            else None 
        }

        def traversePath(turnVect: Vector[Int], index: Int): Option[(Int,Int)] = {
            if (! turnVect.isEmpty) {
                if (turnVect.head == -1) checkSegment(turnVect.tail,(index, index+1))
                else traversePath(turnVect.tail, index+1)
            }
            else None
        }
        traversePath(v, 0)
        
    }

    /**
      * Checks if an EdgePath is a geodesic in a non positive quadrangulation
      *
      * @param path
      * @param nonPosQuad
      * @return
      */
    def isGeodesic(path: EdgePath, nonPosQuad: NonPosQuad): Boolean = {
        assert(edgeVectors(path).toSet.subsetOf(nonPosQuad.edges), s"$path is not a path in the non-positive quadrangulation $nonPosQuad")

        val turnVect = turnPath(path, nonPosQuad)._2

        (isReduced(path)) && (findFirstLeftBracketTurnPath(turnVect) == None) && (findFirstRightBracketTurnPath(turnVect) == None)

    }

    /**
      * Converts a turnPath to standard form, with (1,2,-1,-2) <-> (L,SL,R,SR)
      *
      * @param edge
      * @param turnVect
      * @param nonPosQuad
      * @return
      */
    def turnPathStandardForm(edge: Edge, turnVect: Vector[Int], nonPosQuad: NonPosQuad): Vector[Int] = turnPath(turnPathToEdgePath(edge, turnVect, nonPosQuad), nonPosQuad)._2

    /**
      * Reduces a turnPath
      *
      * @param edge
      * @param turnVect
      * @param nonPosQuad
      * @return
      */
    def turnPathReduce(edge: Edge, turnVect: Vector[Int], nonPosQuad: NonPosQuad): (Edge, Vector[Int]) = {
        if(turnVect.contains(0)) {
            if(turnVect.head == 0) turnPathReduce(nonPosQuad.turnEdge(edge, turnVect(1)), turnVect.tail, nonPosQuad)
            else {
                val i = turnVect.indexOf(0)
                turnPathReduce(edge, ((turnVect.slice(0,i-1) :+ (turnVect(i-1)+turnVect(i+1))) ++ turnVect.slice(i+2,turnVect.size-1)), nonPosQuad)
            }
        }
        else (edge, turnPathStandardForm(edge, turnVect, nonPosQuad))
    }

    /**
      * Helper function for turning a turnPath into a geodesic
      *
      * @param e
      * @param turnVect
      * @param nonPosQuad
      * @return
      */
    def turnPathToGeodesicHelper(edge: Edge, turnVect: Vector[Int], nonPosQuad: NonPosQuad): (Edge, Vector[Int], NonPosQuad) = {
        def rectifyBracket(edge: Edge, vect: Vector[Int], bracket: (Int,Int), correction: Int): Vector[Int] = {
            if(bracket._1 == 0){
                if(bracket._2 == vect.size -1) {
                    val nvect = vect.slice(1, vect.size - 1).map(-_) // SL <-> SR
                    nvect
                }
                else {
                    val nvect = 
                    turnPathStandardForm(edge,
                    //Correcting the bracket and the first turn after the bracket
                    (vect.slice(1, bracket._2).map(-_) :+ (vect(bracket._2+1) + correction)), 
                    nonPosQuad) ++ 
                    //Rest of the turnPath
                    vect.slice(bracket._2 +2, vect.size -1 )
                    nvect
                }
            }
            else {
                val nvect = turnPathStandardForm(edge,
                //Correcting the last turn before the bracket, the bracket and the first turn after the bracket
                ((vect.slice(0, bracket._1-1) :+ (vect(bracket._1-1)+correction)) ++ 
                (vect.slice(bracket._1+1, bracket._2).map(-_) :+ (vect(bracket._2+1) + correction))),
                nonPosQuad) ++
                //Rest of the turnPath
                vect.slice(bracket._2 + 2, vect.size-1)
                nvect
            }
            
        }
        val (e, tvect) = turnPathReduce(edge, turnVect, nonPosQuad)
        val leftBracket = findFirstLeftBracketTurnPath(tvect)
        leftBracket match {
            case None => {
                val rightBracket = findFirstRightBracketTurnPath(tvect)
                rightBracket match {
                    case None => (e, tvect, nonPosQuad)
                    case Some(value) => {
                        val rbracket = value
                        if(rbracket._1 == 0) turnPathToGeodesicHelper(
                            nonPosQuad.SwL(e),
                            rectifyBracket(nonPosQuad.SwL(e), tvect, rbracket, -1),
                            nonPosQuad)
                        else turnPathToGeodesicHelper(
                        e,
                        rectifyBracket(e, tvect, rbracket, -1),
                        nonPosQuad)
                    }
                }
            } 
            case Some(value) => {
                        val lbracket = value
                        if(lbracket._1 == 0) turnPathToGeodesicHelper(e,
                            rectifyBracket(nonPosQuad.SwR(e), tvect, lbracket, 1),
                            nonPosQuad)
                        else turnPathToGeodesicHelper(e,    
                        rectifyBracket(e, tvect, lbracket, 1),
                        nonPosQuad)
            }
        }
    }

    /**
      * Converts a turnPath to a geodesic
      *
      * @param e
      * @param turnVect
      * @param nonPosQuad
      * @return
      */
    def turnPathToGeodesic(e: Edge, turnVect: Vector[Int], nonPosQuad: NonPosQuad): (Edge, Vector[Int]) = {
        val v = turnPathToGeodesicHelper(e, turnVect, nonPosQuad)
        (v._1, v._2)
    }

    /**
      * Reduces an EdgePath
      *
      * @param path
      * @param nonPosQuad
      * @return
      */
      def edgePathReduce(path: EdgePath, nonPosQuad: NonPosQuad): EdgePath = {
          val (e, tvect) = turnPath(path, nonPosQuad)
          val (edge, turnVect) = turnPathReduce(e, tvect, nonPosQuad)
          turnPathToEdgePath(edge, turnVect, nonPosQuad)
      }

    /**
      * Homotopes an EdgePath to a geodesic
      *
      * @param path
      * @param nonPosQuad
      * @return
      */
    def edgePathToGeodesic(path: EdgePath, nonPosQuad: NonPosQuad): EdgePath = {
        val (e, tvect) = turnPath(path, nonPosQuad)
        val geodesic = turnPathToGeodesic(e, tvect, nonPosQuad)
        turnPathToEdgePath(geodesic._1, geodesic._2, nonPosQuad)
    }
}