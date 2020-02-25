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

    lazy val isLoop : Boolean = edgePath.initial == edgePath.terminal

    /** 
     *In case the EdgePath is a loop, shifts the basePoint to the terminal of the first edge
     */
    def shiftBasePoint : EdgePath = {
      require(edgePath.isLoop, s"The EdgePath $edgePath is not a loop. Hence shifting basepoint is not valid")
      val newPath = edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init, last) => Constant(last.initial).+(last).++(init)    
      }
      assert(newPath.isLoop, s"The resulting EdgePath $newPath is not a loop. Hence there is an error in the method definition")
      newPath 
    }

    /** 
     * Reduces a loop to a geodesic
     */
    def loopToGeodesic (twoComplex : TwoComplex) : EdgePath = {
      require(edgePath.inTwoComplex(twoComplex), 
        s"The EdgaPath $edgePath is not inside the TwoComplex $twoComplex")
      require(edgePath.isLoop, s"The EdgePath $edgePath is not a loop. Hence cyclic reduction is not valid")
    
      def helper (path : EdgePath, n : Int) : EdgePath = {
        val corTurnPath = turnPath(path, twoComplex) // the corresponding turnPath
        val firstLeftBracket = findFirstLeftBracketTurnPath(corTurnPath._2)
        val firstRightBracket = findFirstRightBracketTurnPath(corTurnPath._2)
        if (n <= 0) path
        else if (isReduced(path) && (firstLeftBracket == None) && (firstRightBracket == None)) {
          helper(path.shiftBasePoint, n - 1)
        }
        else {
          helper(edgePathToGeodesic(path, twoComplex), length(path) + 3) 
          // only length + 1 should also work. This value is given just for safety.   
        }
      }
      
      helper(edgePath, length(edgePath) + 3) 
      // only length + 1 should also work. This value is given just for safety 
    }

    def verticesCovered : Set[Vertex] = {
      def helper (path : EdgePath, accum : Set[Vertex]) : Set[Vertex] = {
        path match {
          case Constant(vertex) => accum.+(vertex)
          case Append(init, last) => helper(init, accum.++(Set(last.initial, last.terminal)))  
        }
      }
      helper(edgePath, Set())  
    }

    def mod(m : Int, n : Int) : Int = ((m % n) + n) % n 
 
    def intersectionsWith(otherPath : EdgePath , twoComplex : TwoComplex) = Set[(EdgePath, Int)] {
      require(edgePath.inTwoComplex(twoComplex), s"$edgePath is not inside $twoComplex")
      require(otherPath.inTwoComplex(twoComplex), s"$otherPath is not inside $twoComplex")
      require(edgePath.isLoop, s"The method for finding intersections does not work for non-loops such as $edgePath")
      require(otherPath.isLoop, s"The method for finding intersections does not work for non-loops such as $otherPath")
      require(length(edgePath) >= 1, s"The method for finding intersections does not work for 0 length paths such as $edgePath")
      require(length(otherPath) >= 1, s"The method for finding intersections does not work for 0 length paths such as $otherPath")

      val verticesInEdgePath : Set[Vertex] = edgePath.verticesCovered
      val verticesInOtherPath : Set[Vertex] = otherPath.verticesCovered
      val edgePathVector : Vector[Edge] = edgeVectors(edgePath)
      val otherPathVector : Vector[Edge] = edgeVectors(otherPath)
      val thisLength : Int = length(edgePath)
      val thatLength : Int = length(otherPath)
      val intersectionIndices : Set[(Int, Int)] = 
        (0 to (length(edgePath) - 1)).flatMap(i => (0 to (length(otherPath) - 1)).map(j => (i,j))).
        filter(el => (edgePathVector(el._1).initial == otherPathVector(el._2).initial)).toSet
     
      def giveTurnsAtIntersection (i : Int, j : Int) : (Int, Int) = {
        require(edgePathVector(i).initial == otherPathVector(i).initial, s"($i,$j) is not an intersection point")
        val a1 : Edge = edgePathVector(mod(i - 1, thisLength)).flip
        val a2 : Edge = edgePathVector(mod(i, thisLength))
        val b1 : Edge = otherPathVector(mod(j - 1, thatLength)).flip
        val b2 : Edge = otherPathVector(mod(j, thatLength))
        (twoComplex.turnIndex(a1, b1), twoComplex.turnIndex(a2, b2))
      }

      val intersectionsWithTurns : Set[((Int, Int), (Int, Int))] = 
        intersectionIndices.map(el => (el, giveTurnsAtIntersection(el._1, el._2)))
      

      ???
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
    def turnPath(path: EdgePath, twoComplex : TwoComplex): (Edge, Vector[Int]) = {
        require(! edgeVectors(path).isEmpty, s"The edge path $path is empty")
        val v: Vector[Edge] = edgeVectors(path)
        v match{
            case y +: z +: ys => (v.head, v.zip(v.tail :+ v.head).map(v=> twoComplex.turnIndex(v._1,v._2)))
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
    def turnPathToEdgePath(edge: Edge, v: Vector[Int], twoComplex : TwoComplex): EdgePath = {
        def accumTurnPathToEdgePath(edge: Edge, vect: Vector[Int], accum: EdgePath): EdgePath = {
            if (! vect.isEmpty) {
                val e = twoComplex.turnEdge(edge, vect.head)
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
      * @param twoComplex
      * @return
      */
    def isGeodesic(path: EdgePath, twoComplex: TwoComplex): Boolean = {
        assert(edgeVectors(path).toSet.subsetOf(twoComplex.edges), s"$path is not a path in $twoComplex")

        val turnVect = turnPath(path, twoComplex)._2

        (isReduced(path)) && (findFirstLeftBracketTurnPath(turnVect) == None) && (findFirstRightBracketTurnPath(turnVect) == None)

    }

    /**
      * Converts a turnPath to standard form, with (1,2,-1,-2) <-> (L,SL,R,SR)
      *
      * @param edge
      * @param turnVect
      * @param twoComplex
      * @return
      */
    def turnPathStandardForm(edge: Edge, turnVect: Vector[Int], twoComplex : TwoComplex): Vector[Int] = 
      turnPath(turnPathToEdgePath(edge, turnVect, twoComplex), twoComplex)._2

    /**
      * Reduces a turnPath
      *
      * @param edge
      * @param turnVect
      * @param twoComplex
      * @return
      */
    def turnPathReduce(edge: Edge, turnVect: Vector[Int], twoComplex : TwoComplex): (Edge, Vector[Int]) = {
        if(turnVect.contains(0)) {
            if(turnVect.head == 0) turnPathReduce(twoComplex.turnEdge(edge, turnVect(1)), turnVect.tail, twoComplex)
            else {
                val i = turnVect.indexOf(0)
                turnPathReduce(edge, ((turnVect.slice(0,i-1) :+ (turnVect(i-1)+turnVect(i+1))) ++ turnVect.slice(i+2,turnVect.size-1)), twoComplex)
            }
        }
        else (edge, turnPathStandardForm(edge, turnVect, twoComplex))
    }

    /**
      * Helper function for turning a turnPath into a geodesic
      *
      * @param e
      * @param turnVect
      * @param twoComplex
      * @return
      */
    def turnPathToGeodesicHelper(edge: Edge, turnVect: Vector[Int], twoComplex : TwoComplex): (Edge, Vector[Int], TwoComplex) = {
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
                    twoComplex) ++ 
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
                twoComplex) ++
                //Rest of the turnPath
                vect.slice(bracket._2 + 2, vect.size-1)
                nvect
            }
            
        }
        val (e, tvect) = turnPathReduce(edge, turnVect, twoComplex)
        val leftBracket = findFirstLeftBracketTurnPath(tvect)
        leftBracket match {
            case None => {
                val rightBracket = findFirstRightBracketTurnPath(tvect)
                rightBracket match {
                    case None => (e, tvect, twoComplex)
                    case Some(value) => {
                        val rbracket = value
                        if(rbracket._1 == 0) turnPathToGeodesicHelper(
                            twoComplex.SwL(e),
                            rectifyBracket(twoComplex.SwL(e), tvect, rbracket, -1),
                            twoComplex)
                        else turnPathToGeodesicHelper(
                        e,
                        rectifyBracket(e, tvect, rbracket, -1),
                        twoComplex)
                    }
                }
            } 
            case Some(value) => {
                        val lbracket = value
                        if(lbracket._1 == 0) turnPathToGeodesicHelper(e,
                            rectifyBracket(twoComplex.SwR(e), tvect, lbracket, 1),
                            twoComplex)
                        else turnPathToGeodesicHelper(e,    
                        rectifyBracket(e, tvect, lbracket, 1),
                        twoComplex)
            }
        }
    }

    /**
      * Converts a turnPath to a geodesic
      *
      * @param e
      * @param turnVect
      * @param twoComplex
      * @return
      */
    def turnPathToGeodesic(e: Edge, turnVect: Vector[Int], twoComplex : TwoComplex): (Edge, Vector[Int]) = {
        val v = turnPathToGeodesicHelper(e, turnVect, twoComplex)
        (v._1, v._2)
    }

    /**
      * Reduces an EdgePath
      *
      * @param path
      * @param twoComplex
      * @return
      */
      def edgePathReduce(path: EdgePath, twoComplex : TwoComplex): EdgePath = {
          val (e, tvect) = turnPath(path, twoComplex)
          val (edge, turnVect) = turnPathReduce(e, tvect, twoComplex)
          turnPathToEdgePath(edge, turnVect, twoComplex)
      }

    /**
      * Homotopes an EdgePath to a geodesic
      *
      * @param path
      * @param twoComplex
      * @return
      */
    def edgePathToGeodesic(path: EdgePath, twoComplex : TwoComplex): EdgePath = {
        val (e, tvect) = turnPath(path, twoComplex)
        val geodesic = turnPathToGeodesic(e, tvect, twoComplex)
        turnPathToEdgePath(geodesic._1, geodesic._2, twoComplex)
    }
}