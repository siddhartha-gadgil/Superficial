package superficial

import NonPosQuad._
import Intersection._
import Edge._

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

    def mod(m : Int, n : Int) : Int = ((m % n) + n) % n 
 
    /**
     * Given indexes i and j gives the subpath between i-th and j-th vertex. 
     * Because it works on loops j can be less than i.
     */
    def cyclicalTake(i : Int, j : Int) : EdgePath = {
      require(edgePath.isLoop, s"Method is only valid for loops")
      val len : Int = length(edgePath)
      require(((i < len )&&(i >= 0)), s"$i is not in valid range")
      require(((j < len)&&(j >= 0)), s"$j is not in valid range")
      val edgesVect : Vector[Edge] = edgeVectors(edgePath)
      edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init, last) => {
          if (i == j) Constant(edgesVect(i).initial)
          else if (i < j) EdgePath.apply(edgesVect.slice(i, j))
          else /*if (i > j)*/ EdgePath.apply(edgesVect.slice(i, len).++(edgesVect.slice(0, j)))
        }
      }     
    }

    /** 
     * In case the EdgePath is a loop, shifts the basePoint to the terminal of the first edge.
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
    def loopToGeodesic (nonposQuad : NonPosQuad) : EdgePath = {
      require(edgePath.inTwoComplex(nonposQuad), 
        s"The EdgePath $edgePath is not inside the TwoComplex $nonposQuad")
      require(edgePath.isLoop, s"The EdgePath $edgePath is not a loop. Hence cyclic reduction is not valid")
    
      def helper (path : EdgePath, n : Int) : EdgePath = {
        path match {
          case Constant(initial) => path
          case Append(init, last) => {
            val corTurnPath = turnPath(path, nonposQuad) // the corresponding turnPath
            val firstLeftBracket = findFirstLeftBracketTurnPath(corTurnPath._2)
            val firstRightBracket = findFirstRightBracketTurnPath(corTurnPath._2)
            if (n <= 0) path
            else if (isReduced(path) && (firstLeftBracket == None) && (firstRightBracket == None)) {
              helper(path.shiftBasePoint, n - 1)
            }
            else {
              helper(edgePathToGeodesic(path, nonposQuad), length(path) + 3) 
              // only length + 1 should also work. This value is given just for safety.   
            }
          }
        }
        
      }
      
      helper(edgePath, length(edgePath) + 3) 
      // only length + 1 should also work. This value is given just for safety 
    }

    /**
     * Gives the set of vertices visited by the path.
     */
    def verticesCovered : Set[Vertex] = {
      def helper (path : EdgePath, accum : Set[Vertex]) : Set[Vertex] = {
        path match {
          case Constant(vertex) => accum.+(vertex)
          case Append(init, last) => helper(init, accum.++(Set(last.initial, last.terminal)))  
        }
      }
      helper(edgePath, Set())  
    }

    /**
     * Given another loop gives intersection along with signs.
     */
    def intersectionsWith(otherPath : EdgePath , twoComplex : TwoComplex) : Set[Intersection] = {
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

      // Intersection point of two paths
      val intersectionIndices : Set[(Int, Int)] = 
        ((0 to (length(edgePath) - 1)).flatMap(i => (0 to (length(otherPath) - 1)).map(j => (i,j)))).
        filter(el => (edgePathVector(el._1).initial == otherPathVector(el._2).initial)).toSet
     
      // Adds turn information on both side of the intersection
      def giveTurnsAtIntersection (i : Int, j : Int) : (Int, Int) = {
        require(edgePathVector(i).initial == otherPathVector(j).initial, s"($i,$j) is not an intersection point")
        val a1 : Edge = edgePathVector(mod(i - 1, thisLength))
        val a2 : Edge = edgePathVector(mod(i, thisLength)).flip
        val b1 : Edge = otherPathVector(mod(j - 1, thatLength))
        val b2 : Edge = otherPathVector(mod(j, thatLength)).flip
        (twoComplex.angleBetween(a1, b1), twoComplex.angleBetween(a2, b2))
      }

      // The elements are of the form ((i,j), (u,v)) where
      // i-th and j-th edge of edgePath and otherPath have same initial vertex
      // u is the turn b/w (i-1)-th edge of edgePath and (j-1)-th edge of otherPath (technically their flips).
      // v is the turn b/w i-th edge of edgePath and j-th edge of otherPath 
      val intermediateInter : Set[((Int, Int), (Int, Int))] = 
        intersectionIndices.map(el => (el, giveTurnsAtIntersection(el._1, el._2)))
      val unmerged : Vector[Intersection] = 
        intermediateInter.map(el => Intersection.apply(el._1, el._1, el._2._1, el._2._2)).toVector
      val merged : Set[Intersection] = Intersection.mergeAll(unmerged, thisLength, thatLength).toSet   
      merged
    }

    /**
     * Gives self intersections with signs.
     */
    def selfIntersection (nonposQuad : NonPosQuad) : Set[Intersection] = {
      val leftmostPath = canoniciseLoop(edgePath, nonposQuad)
      val rightmostPath = (canoniciseLoop(edgePath.reverse, nonposQuad)).reverse
      leftmostPath.intersectionsWith(rightmostPath, nonposQuad)
    }

    /**
      * Extracts the start and end indices of the crossings of the given sign 
      * between the EdgePath (if it is a loop, error otherwise) and a given loop
      *
      * @param loop
      * @param nonposQuad
      * @param sign
      * @return
      */
    def extractCrossings(loop: EdgePath, nonposQuad: NonPosQuad, sign: Int): Vector[((Int, Int), (Int,Int))] = {
      if(loop != edgePath) {
        val canonicalLoop = canoniciseLoop(edgePath, nonposQuad)
        val otherCanonicalLoop = canoniciseLoop(loop, nonposQuad)
        val vect = (canonicalLoop.intersectionsWith(otherCanonicalLoop, nonposQuad)).toVector
        (vect.filter((x: Intersection) => ((x.getSign(canonicalLoop, otherCanonicalLoop, nonposQuad)) == sign))).
        map((x: Intersection)=> (x.start, x.end))
      }
      else {
        val vect = (edgePath.selfIntersection(nonposQuad)).toVector
        val leftmostPath = canoniciseLoop(edgePath, nonposQuad)
        val rightmostPath = (canoniciseLoop(edgePath.reverse, nonposQuad)).reverse
        (vect.filter((x: Intersection) => ((x.getSign(leftmostPath, rightmostPath, nonposQuad)) == sign))).
        map((x: Intersection)=> (x.start, x.end))
      }
        
    }

    /**
      * Extracts the start and end indices of all positive crossings 
      * between the canonical forms of the EdgePath (if it is a loop, error otherwise) and a given loop
      *
      * @param loop
      * @param nonposQuad
      * @return
      */
    def positiveCrossings(loop: EdgePath, nonposQuad: NonPosQuad): Vector[((Int, Int), (Int,Int))] ={
      edgePath.extractCrossings(loop, nonposQuad, 1)
    }

    /**
      * Extracts the start and end indices of all negative crossings
      * between the canonical forms of the EdgePath (if it is a loop, error otherwise) and a given loop
      *
      * @param loop
      * @param nonposQuad
      * @return
      */
    def negativeCrossings(loop: EdgePath, nonposQuad: NonPosQuad): Vector[((Int, Int), (Int,Int))] = {
      edgePath.extractCrossings(loop, nonposQuad, -1)
    }

    /**
      * Extracts the start and end indices of all non-crossings
      * between the canonical forms EdgePath (if it is a loop, error otherwise) and a given loop
      *
      * @param loop
      * @param twoComplex
      * @return
      */
    def nonCrossings(loop: EdgePath, nonposQuad: NonPosQuad): Vector[((Int, Int), (Int,Int))] = {
      edgePath.extractCrossings(loop, nonposQuad, 0) 
    }

    /**
      * Calculates the geometric intersection number of the EdgePath and a given path
      *
      * @param path
      * @param nonposQuad
      * @return
      */
    def GIN(path: EdgePath, nonposQuad: NonPosQuad): Int = {
      if(edgePath.isLoop && path.isLoop) 
        (edgePath.positiveCrossings(path, nonposQuad)).size + (edgePath.negativeCrossings(path, nonposQuad)).size
      else 0
    }

    /**
      * Calculates the geometric self-intersection number of a curve
      *
      * @param nonposQuad
      * @return
      */
    def selfGIN(nonposQuad: NonPosQuad): Int = {
      edgePath.GIN(edgePath, nonposQuad)
    }

    /**
      * Calculates the algebraic intersection number of the EdgePath and a given path
      *
      * @param path
      * @param nonposQuad
      * @return
      */
    def AIN(path: EdgePath, nonposQuad: NonPosQuad): Int = {
      if(edgePath.isLoop && path.isLoop) 
        (edgePath.positiveCrossings(path, nonposQuad)).size - (edgePath.negativeCrossings(path, nonposQuad)).size
      else 0
    }

    /**
      * Calculates the geometric self-intersection number of a curve
      *
      * @param path
      * @param nonposQuad
      * @return
      */
    def selfAIN(path: EdgePath, nonposQuad: NonPosQuad): Int = {
      edgePath.AIN(edgePath, nonposQuad)
    }      

    /**
      * Checks whether two paths are homotopic fixing endpoints
      *
      * @param path
      * @param twoComplex
      * @return
      */
    def isHomotopicTo(path: EdgePath, nonposQuad: NonPosQuad): Boolean = {
      require(path.inTwoComplex(nonposQuad), s"The path $path is not in the Two Complex $nonposQuad")
      require(edgePath.inTwoComplex(nonposQuad), s"The path $edgePath is not in the Two Complex $nonposQuad")
      require((initial == path.initial), s"The path $path doesn't have the same initial vertex as $edgePath")
      require((terminal == path.terminal), s"The path $path doesn't have the same terminal vertex as $edgePath")

      val newPath = path ++ (edgePath.reverse)
      val mergedGeodesic = edgePathToGeodesic(newPath, nonposQuad)
      mergedGeodesic match {
        case Constant(vertex) => true
        case _ => false
      }
    }

    def isFreelyHomotopicTo(loop: EdgePath, nonposQuad: NonPosQuad): Boolean = {
      require(loop.inTwoComplex(nonposQuad), s"The path $loop is not in the Two Complex $nonposQuad")
      require(edgePath.inTwoComplex(nonposQuad), s"The path $edgePath is not in the Two Complex $nonposQuad")
      require(loop.isLoop, s"The path $loop is not a loop")
      require(edgePath.isLoop, s"The path $edgePath is not a loop")

      def checkSameCanonicalLoop(loop1: EdgePath, loop2: EdgePath): Boolean = {
        def checkSameCanonicalLoopHelper(loop1: EdgePath, loop2: EdgePath, n: Int): Boolean = {
          if(n<=0) false
          else (loop1 == loop2) || checkSameCanonicalLoopHelper(loop1.shiftBasePoint, loop2, n-1)
        }
        val n = length(loop1)
        (n == length(loop2)) && (checkSameCanonicalLoopHelper(loop1, loop2, n+2))
      } 

      val canonical1 = canoniciseLoop(edgePath, nonposQuad)
      val canonical2 = canoniciseLoop(loop, nonposQuad)

      canonical1 match {
        case Constant(initial) => canonical2 match {
          case Constant(initial) => true
          case Append(init, last) => false
        }
        case Append(init, last) => canonical2 match {
          case Constant(initial) => false
          case Append(init1, last1) => checkSameCanonicalLoop(canonical1, canonical2)
        }
      }
    }

    def isEqualUptoBasepointShiftHelper (otherPath : EdgePath, remains : Int, current : Int) : (Boolean, Int) = {
      // the requirement inside isEqualUptoBasepointShift should ensure that both edgePath and otherPath
      // are loops    
      if (remains < 0) (false, 0)
      else if (edgePath == otherPath) (true, current)
      else isEqualUptoBasepointShiftHelper(otherPath.shiftBasePoint, remains - 1, current + 1)
    }

    /**
     * Checks if otherPath is equal to edgePath upto shift of basepoint.
     * Should return (true, n) if otherPath after n shifts of basepoints is equal to edgePath
     */
    def isEqualUptoBasepointShift (otherPath : EdgePath) : (Boolean, Int) = {

      require(edgePath.isLoop, s"method is not useful for non-loops such as $edgePath")
      require(otherPath.isLoop, s"method is not useful for non-loops such as $otherPath")

      isEqualUptoBasepointShiftHelper(otherPath, length(edgePath) + 1, 0)
    }

    def makeBasePointSameHelper (path : EdgePath): EdgePath = {
        // the requirement inside makeBasePointSame should ensure that edgePath is a loop       
        if (edgePath.initial == path.initial) path
        else makeBasePointSame(path.shiftBasePoint)
      }

    /**
     * Given a path otherPath shifts otherPath until it has the same basepoint as edgePath
     */
    def makeBasePointSame (otherPath : EdgePath) = {
      require(edgePath.isLoop, s"method is not useful for non-loops such as $edgePath")
      require(otherPath.isLoop, s"method is not useful for non-loops such as $otherPath")
      
      val sameVertices : Set[Vertex] = 
        edgePath.verticesCovered.intersect(otherPath.verticesCovered)
 
      require(sameVertices.nonEmpty, s"$edgePath and $otherPath don't have any vertices in common.")
      makeBasePointSameHelper(otherPath)       
    } 

    /* 
     * Given a vertex, gives the first index at which it appears in edgePath. 
     * If not present returns None.
     */
    def findVertexIndex (vertex : Vertex) : Option[Int] = {
      edgePath.reverse match {
        case Constant(u) => if (vertex == u) Some(0) else None
        case Append(init, last) => {
          if (last.terminal == vertex) Some(0)
          else init.reverse.findVertexIndex(vertex).map(n => (n + 1))
        }
      }
    }

    def isPrimitiveLoopHelper (accum : Set[Vertex]) : Boolean = {
      edgePath match {
        case Constant(v) => true
        case Append(init, last) =>
           if (accum.contains(last.terminal)) false
           else init.isPrimitiveLoopHelper(accum.+(last.terminal)) 
      }
    }

    def isPrimitiveLoop : Boolean = {
      require(edgePath.isLoop, s"$edgePath is not a loop.")
      edgePath.isPrimitiveLoopHelper(Set())
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
        path match {
          case Constant(initial) => (makeEmptyEdge(initial), Vector[Int]())
          case Append(init, last) => {
            val v: Vector[Edge] = edgeVectors(path)
            v match{
              case (y +: z +: ys) => (v.head, (v.zip(v.tail)).map(v=> twoComplex.turnIndex(v._1,v._2)))
              case Vector(y) => (y, Vector[Int]())
            }
          }  
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
      if (edge.isEmpty == true) Constant(edge.initial)
      else {
          def accumTurnPathToEdgePath(edge: Edge, vect: Vector[Int], accum: EdgePath): EdgePath = {
            if (! vect.isEmpty) {
                val e = twoComplex.turnEdge(edge, vect.head)
                accumTurnPathToEdgePath(e, vect.tail, Append(accum, e))
            }
            else accum
          }
          accumTurnPathToEdgePath(edge, v, Append(Constant(edge.initial), edge))
        } 
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
    def turnPathReduce(edge: Edge, turnVect1: Vector[Int], twoComplex : TwoComplex): (Edge, Vector[Int]) = {
      if (edge.isEmpty == true) (edge, turnVect1)
      else {
        val turnVect = turnPathStandardForm(edge, turnVect1, twoComplex)
        if(turnVect.contains(0)) {
          if(turnVect == Vector(0)) {
            (makeEmptyEdge(edge.initial), Vector())
          }
          else {
            if(turnVect.head == 0) turnPathReduce(twoComplex.turnEdge(edge.flip, turnVect(1)), (turnVect.tail).tail, twoComplex)
            else if(turnVect.last == 0) turnPathReduce(edge, (turnVect.init).init, twoComplex)
            else {
                val i = turnVect.indexOf(0)
                turnPathReduce(edge, 
                (turnVect.slice(0,i-1) :+ (turnVect(i-1) + turnVect(i+1))) ++ turnVect.slice(i+2,turnVect.size),
                twoComplex)
            }
          }
            
        }
        else (edge, turnPathStandardForm(edge, turnVect, twoComplex))
      }
        
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
      if (edge.isEmpty == true) (edge, turnVect, twoComplex)
      else {
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

    def findQuad(e1: Edge, e2: Edge, nonposQuad: NonPosQuad): Polygon = {
      val PolySet = ((nonposQuad.facesWithEdge(e1)) union (nonposQuad.facesWithEdge(e1.flip))) intersect (
      (nonposQuad.facesWithEdge(e2)) union (nonposQuad.facesWithEdge(e2.flip))
      )
      assert(! PolySet.isEmpty, s"There is no face containing both one of $e1 and its flip, and one of $e2 and flip")
      PolySet.toVector(0)
    }

    def shiftRightwards(e1: Edge, e2: Edge, nonposQuad: NonPosQuad): Vector[Edge] = {
      val quad = findQuad(e1, e2, nonposQuad)
      val edgeVect = (quad.edges -- Set(e1, e1.flip, e2, e2.flip)).toVector
      val initVect = (edgeVect.flatMap(x => Vector(x, x.flip)))
      val tryVect = initVect.flatMap(x => initVect.map((x, _)).filter(x => x._1.terminal == x._2.initial))
      val rightVect = tryVect.filter(x => ((nonposQuad.turnIndex(x._1, x._2) == 1) && (x._1.initial == e1.initial)))
      assert(! rightVect.isEmpty, s"There is no right turn correction for the edges $e1 and $e2")
      val rightEdges = rightVect.head
      Vector(rightEdges._1, rightEdges._2)
    }

    def canoniciseVectorLoop(eVect: Vector[Edge], nonposQuad: NonPosQuad): Vector[Edge] = {
      val turnVect = turnPath(EdgePath.apply(eVect), nonposQuad)._2
      val i = turnVect.indexOf(-1)
      if(i == -1) eVect
      else {
        val newVect = eVect.slice(0,i) ++ 
        shiftRightwards(eVect(i), eVect(i+1), nonposQuad) ++ 
        eVect.slice(i+2,eVect.length)
        canoniciseVectorLoop(newVect, nonposQuad)
      }
    }

    def canoniciseLoopHelper(loop: EdgePath, n: Int, nonposQuad: NonPosQuad): EdgePath = {
        if (n<=0) loop
        else {
          loop match {
            case Constant(initial) => loop
            case Append(init, last) => {
              val loop1 = loop.loopToGeodesic(nonposQuad)
              val eVect = edgeVectors(loop1)
              val newVect = canoniciseVectorLoop(eVect, nonposQuad)
              val newLoop = EdgePath.apply(newVect)
              require(isGeodesic(newLoop, nonposQuad), s"The loop $newLoop is not a geodesic")
              canoniciseLoopHelper(newLoop.shiftBasePoint, n-1, nonposQuad)
            }
          }
          
        }
      }
      
    def canoniciseLoop(loop: EdgePath, nonposQuad: NonPosQuad): EdgePath = {
      require(loop.isLoop, s"The path $loop is not a loop")
      val edgeVect = edgeVectors(loop)
      
      canoniciseLoopHelper(loop, length(loop)+2, nonposQuad)
    }
    
    def isGeodesicLoop(loop: EdgePath, nonposQuad: NonPosQuad): Boolean = {
      require(loop.isLoop, s"The path $loop is not a loop")
      def isGeodesicLoopHelper(loop: EdgePath, n: Int, nonposQuad: NonPosQuad): Boolean = {
        if (n<=0) true
        else{
          isGeodesic(loop, nonposQuad) match {
            case true => isGeodesicLoopHelper(loop, n-1, nonposQuad)
            case false => false
          }
        }
      }
      isGeodesicLoopHelper(loop, length(loop)+2, nonposQuad)
    }

    def isCanonicalGeodesicLoop(loop: EdgePath, nonposQuad: NonPosQuad): Boolean = {
      require(loop.isLoop, s"The path $loop is not a loop")

      def isCanonicalGeodesicLoopHelper(loop: EdgePath, n: Int, nonposQuad: NonPosQuad): Boolean = {
        if (n<=0) true
        else{
          (turnPath(loop, nonposQuad)._2).contains(-1) match {
            case false => isCanonicalGeodesicLoopHelper(loop, n-1, nonposQuad)
            case true => false
          }
        }
      }
      isCanonicalGeodesicLoopHelper(loop, length(loop)+2, nonposQuad) && isGeodesic(loop, nonposQuad)
    }

}

/*
{
  
import superficial._
val g = new StandardSurface(2)
val (quad, (f, b)) = Quadrangulation.quadrangulate(g)
val quad1 = NonPosQuad(quad)
val p1 = g.getPath("a1a2")
val path1 = f(p1)
val p2 = g.getPath("b1b2")
val path2 = f(p2)
import EdgePath._
val ppath = path1 ++ path1.reverse
val tpath = turnPath(ppath, quad1)

}

val canonicalPath1 = EdgePath.canoniciseLoop(path1, quad1)

*/