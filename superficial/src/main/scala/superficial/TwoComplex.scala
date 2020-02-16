package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a
import EdgePath._

/**
  * Abstract polygon, with given edges and vertices, i.e. a two-complex with a single face.
  * @param sides number of sides
  */
trait Polygon extends TwoComplex {
  val sides: Int

  lazy val faces = Set(this)

  lazy val indices: Vector[Index] = (0 until sides).toVector

  val boundary: Vector[Edge]

  lazy val basePoint : Vertex = boundary.head.initial // override for empty boundary

  def checkBoundary =
    Polygon.checkBoundary(boundary) // not asserted here because of possible delayed initialization

  def checkPoly: Boolean =
    checkBoundary &&
      (sides == boundary.size) &&
      (edges.forall(_.checkFlip)) &&
      (edges == boundary.toSet.flatMap((e: Edge) => Set(e, e.flip))) &&
      (vertices == edges.map(_.initial))

  /**
    * the boundary as a formal sum
    */
  def del: FormalSum[Edge] = {
    val ob = boundary.collect { case oe: OrientedEdge => oe }
    assert(
      ob.size == boundary.size,
      "computing boundary where some edge is not oriented"
    )
    val coeffVec = ob.map {
      case e => if (e.positivelyOriented) (e, 1) else (e.flip, -1)
    }
    FormalSum.reduced(coeffVec)
  }

  /**
    * the set of edges
    */
  lazy val edges: Set[Edge] =
    boundary.toSet.flatMap((s: Edge) => Set(s, s.flip))

  /**
    * the set of vertices
    */
  val vertices: Set[Vertex]

  def boundaryIndex(e: Edge): Set[(Int, Boolean)] =
    indices.toSet
      .filter { (n) =>
        e == boundary(n) || e.flip == boundary(n)
      }
      .map(n => (n, e == boundary(n)))    
}

object Polygon {
  /** Checks that for each conseutive edges e1 and e2 e1.terminal == e2.intial */ 
  def checkBoundary(v: Vector[Edge]) =
    v.zip(v.tail).forall { case (e1, e2) => e1.terminal == e2.initial } && (
      v.last.terminal == v.head.initial
    )

  type Index = Int

  /**
    * Construction of a polygon given number of sides
    * @param n number of sides
    * @return a polgon with n sides
    */
  def apply(n: Int): Polygon = new Polygon { self =>
    val sides = n

    lazy val boundary: Vector[Edge] =
      for (e <- indices)
        yield PolygonEdge(self, e, positiveOriented = true)

    lazy val vertices: Set[Vertex] =
      (indices map (PolygonVertex(self, _))).toSet     
  }

  def apply(bdy: Vector[Edge]): Polygon = {
    require(bdy.nonEmpty, s"use `degenerate` method for empty polygon")
    assert(checkBoundary(bdy), s"boundary $bdy not a loop")
    new Polygon {
      val sides: Int = bdy.size
      val boundary: Vector[Edge] = bdy
      val vertices: Set[Vertex] = bdy.map(_.initial).toSet
    }
    
  }

  def degenerate(v: Vertex): Polygon = new Polygon {
    val sides: Int = 0
    val boundary: Vector[Edge] = Vector()
    val vertices: Set[Vertex] = Set(v)
    override lazy val basePoint: Vertex = v
  }

  case class Symbolic(name: String, boundary: Vector[Edge]) extends Polygon {
    val sides: Int = boundary.size

    val vertices: Set[Vertex] = edges.flatMap(e => Set(e.initial, e.terminal))
  }

  case class PolygonEdge(
      polygon: Polygon,
      index: Index,
      positiveOriented: Boolean
  ) extends Edge {
    lazy val flip: PolygonEdge = PolygonEdge(polygon, index, !positiveOriented)

    lazy val terminal: PolygonVertex =
      if (positiveOriented)
        PolygonVertex(polygon, (index + 1) % polygon.sides)
      else
        PolygonVertex(polygon, index)

    lazy val initial: PolygonVertex =
      if (!positiveOriented)
        PolygonVertex(polygon, (index + 1) % polygon.sides)
      else
        PolygonVertex(polygon, index)

  }

  case class PolygonVertex(polygon: Polygon, index: Index) extends Vertex

}

object TwoComplex {
  def pure(fs: Polygon*): TwoComplex = {
    fs.foreach(f => assert(f.checkBoundary))
    new PureTwoComplex {
      val faces: Set[Polygon] = fs.toSet
    }
  }
  case class Concrete(vertices: Set[Vertex], edges: Set[Edge], faces: Set[Polygon])
      extends TwoComplex

  @annotation.tailrec
  def halfEdges(edges: List[Edge], accum: Set[Edge]): Set[Edge] = edges match {
    case head :: next =>
      if (accum.intersect(Set(head, head.flip)).nonEmpty) halfEdges(next, accum)
      else halfEdges(next, accum + head)
    case Nil => accum
  }

  // collapse all edges that are not loops
  def allCollapsed(complex: TwoComplex): TwoComplex =
    nonLoop(complex)
      .map { e =>
        allCollapsed(complex.collapseEdge(e)._1)
      }
      .getOrElse(complex).ensuring{_.checkComplex}

  def nonLoop(complex: TwoComplex): Option[Edge] =
    complex.edges.find(edge => edge.initial != edge.terminal)

  def mergeFaces(e: Edge, first: Polygon, second: Polygon): Polygon = {
    require(first.boundary.contains(e))
    require(second.boundary.contains(e.flip))
    require(first != second)
    val firstHead = first.boundary.takeWhile(_ != e) // edges before e
    val firstTail = first.boundary.drop(firstHead.size + 1) // edges after e
    val secondHead = second.boundary.takeWhile(_ != e.flip) // edges before e
    val secondTail = second.boundary.drop(secondHead.size + 1) // edges after e
    val bdy = firstHead ++ secondTail ++ secondHead ++ firstTail 
    if (bdy.nonEmpty) Polygon(bdy) else Polygon.degenerate(e.initial)
  }.ensuring(poly => poly.checkPoly)

  /**
    * Symbolic two-complexes, i.e. two-complexes with vertices, edges and faces determined by their names.
    * 
    * For example, here is how a torus is constructed.
    * 
    * {{{
    * TwoComplex.symbolic(
    *     "x")(
    *     "a" -> ("x", "x"), "b" -> ("x", "x"))(
    *     "face" -> Seq("a" -> true, "b" -> true, "a" -> false, "b" -> false)
    *     ) 
    * }}}
    *
    * @param vertexNames names of vertices separated by commas,
    * @param edgeMap edgepair names mapped to initial and terminal vertices
    * @param faceMap face-names mapped to pairs (edge-name, isPositivelyOriented)
    * @return two-complex determined by the data
    */
  def symbolic(vertexNames: String*)(edgeMap: (String, (String, String))*)(
      faceMap: (String, Seq[(String, Boolean)])*
  ): TwoComplex = {
    val vertices: Set[Vertex] = vertexNames.toSet.map(Vertex.Symbolic(_))
    val edges: Set[Edge] =
      edgeMap.toMap.flatMap {
        case (e, (a, b)) =>
          val ed = Edge.symbolic(e, a, b)
          Set(ed, ed.flip)
      }.toSet

    def getEdge(s: String, pos: Boolean) = edgeMap.find(_._1 == s).map {
      case (e, (a, b)) => Edge.symbolic(e, a, b, pos)
    }

    val faces: Set[Polygon] =
      faceMap.map {
        case (face, vec) =>
          val edges = for {
            (name, pos) <- vec
          } yield (getEdge(name, pos).get)
          Polygon.Symbolic(face, edges.toVector)
      }.toSet
    Concrete(vertices, edges, faces)
  }
}

/**
  *  A polyheadral two complex, with faces polygons, a collection of edges and
  */
trait TwoComplex { twoComplex =>
  val faces: Set[Polygon]

  val edges: Set[Edge] // these come in pairs, related by flip (reversing orientation)

  def checkComplex =
    faces.forall(_.checkBoundary) && edges.forall(_.checkFlip) && edges
      .map(_.initial)
      .subsetOf(vertices)

  lazy val positiveEdges: Vector[OrientedEdge] =
    edges.toVector.collect {
      case oe: OrientedEdge if oe.positivelyOriented => oe}  

  // to take care of unoriented edges
  lazy val halfEdges: Set[Edge] =
    TwoComplex.halfEdges(
      (edges -- positiveEdges.toSet).toList,
      positiveEdges.toSet
    )
  

  def edgeIndex(edge: Edge) : Option[(Int, Boolean)] = {
    halfEdges.zipWithIndex
      .find { case (e, i) => e == edge || e.flip == edge }
      .map { case (e, i) => (i, e == edge) }
  }

  val vertices: Set[Vertex]

  lazy val chi = vertices.size - (edges.size / 2) + faces.size

  lazy val indexedVertices = vertices.zipWithIndex // for fixing order

  def vertexIndex(v: Vertex) = indexedVertices.find(_._1 == v).map(_._2)

  def facesWithEdge(edge: Edge): Set[Polygon] =
    faces.filter((face) => face.edges.contains(edge))

  def edgeIndices(edge: Edge): Set[(Polygon, Index, Boolean)] =
    faces.flatMap(
      (f) => f.boundaryIndex(edge).map { case (n, flipped) => (f, n, flipped) }
    )

  def normalArcs: Set[NormalArc] =
    for {
      face <- faces
      initial <- face.indices
      terminal <- face.indices
    } yield NormalArc(initial, terminal, face)

  /*
   * Returns the result of collapasing an edge of the twoComplex.
   * Also returns a pair of methods. The first of them takes edgePaths in the original TwoComplex
   * and returns edgepaths in the TwoComplex with the edge collapsed.
   * The other method is also an edgePath map but in the opposite direction. That is it takes
   * edgePaths in the resulting TwoComplex and returns an edgePath in the original twoComplex.
   **/
  def collapseEdge(e: Edge): (TwoComplex, (EdgePath => EdgePath, EdgePath => EdgePath)) = {
    require(e.initial != e.terminal, s"cannot collapse loop $e at ${e.initial}")
    // map from edges to new edges
    val newEdgeHalfMap: Map[Edge, Edge] =
      halfEdges
        .filterNot(Set(e, e.flip).contains(_))
        .map { edge =>
          val newChap: Edge =
            if (Set(edge.initial, edge.terminal).contains(e.terminal)) {
              val initial: Vertex =
                if (edge.initial == e.terminal) e.initial else edge.initial
              val terminal: Vertex =
                if (edge.terminal == e.terminal) e.initial else edge.terminal
              new EdgePair(initial, terminal).Positive
            } else edge // the new chap is the old one
          edge -> newChap
        }
        .toMap

    val newEdgeMap: Map[Edge, Edge] =
      newEdgeHalfMap ++
        newEdgeHalfMap.map {
          case (k, v) => (k.flip, v.flip)
        }

    val newEdgeBackMap : Map[Edge, Edge] = newEdgeMap.map(_.swap)    

    def newPoly(polygon: Polygon): Polygon =
      new Polygon {
        val sides: Int = polygon.sides - 1
        val boundary: Vector[Edge] =
          polygon.boundary.filterNot(Set(e, e.flip).contains(_)).map { edge =>
            newEdgeMap(edge)
          }
        val vertices: Set[Vertex] = polygon.vertices - e.terminal
        assert(checkBoundary)
      }

    object newComplex extends TwoComplex {
      val edges: Set[Edge] = newEdgeMap.values.toSet
      val faces: Set[Polygon] = twoComplex.faces.map(newPoly(_))
      val vertices: Set[Vertex] = twoComplex.vertices - e.terminal
      override def toString(): String = s"$twoComplex/$e @ $hashCode"
    }

    def forwardEdgePathMap (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(twoComplex), s"$edgePath is not a path in $twoComplex")
      val newPath : EdgePath = edgePath match {
        case Constant(vertex) => {
          if (vertex == e.terminal) Constant(e.initial) else Constant(vertex)
          }
        case Append(init, last) => {
          if (last == e) forwardEdgePathMap(init) else forwardEdgePathMap(init).+(newEdgeMap(last))
          }    
        } 
      assert(newPath.inTwoComplex(newComplex), s"$newPath is not a path in $newComplex")
      newPath 
    }

    def backwardEdgePathMap (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(newComplex), s"$edgePath is not a path in $newComplex")
      val newPath : EdgePath = edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init, last) => {
          val backInit : EdgePath = backwardEdgePathMap(init)
          val backLast : Edge = newEdgeBackMap(last)
          if (backInit.terminal == e.initial && backLast.initial == e.terminal) backInit.+(e).+(backLast)
          else if (backInit.terminal == e.terminal && backLast.initial == e.initial) backInit.+(e.flip).+(backLast)
          else {
            assert(backInit.terminal == backLast.initial, 
              s"${backInit.terminal} is not equal to ${backLast.initial} and they don't differ by the collapsed edge $e")
            backInit.+(backLast)   
          } 
        }
      }
      assert(newPath.inTwoComplex(twoComplex), s"$newPath is not a path in $twoComplex")
      newPath
    }

    (newComplex, (forwardEdgePathMap, backwardEdgePathMap))
  }

  /**
    * Finds neighbours of a vertex
    *
    * @param v the vertex
    * @return set of neighbours
    */ 
  def vertexNbr(v: Vertex): Set[Vertex] = {
    val s = (twoComplex.edges.filter(_.initial == v).map(_.terminal)).
        union(twoComplex.edges.filter(_.terminal == v).map(_.initial))
    s+v
  }

  //Collects a first order neighbourhood of a set onto a set
  def setNbr(s: Set[Vertex]): Set[Vertex] = {
    s.flatMap(vertexNbr(_))
  }

  //Finds the maximal set of neighbours of a given set
  def maxSetNbr(s: Set[Vertex]): Set[Vertex] = {
    if (setNbr(s) == s) s
    else maxSetNbr(setNbr(s))
  }

  /**
    * Finds the connected component of a vertex
    *
    * @param v the starting vertex
    * @return set of initial neighbours
    */ 
  def connectedComponent(v: Vertex): Set[Vertex] = {
    maxSetNbr(Set(v))
  }

  /**
    * Checks if the complex is connected
    *
    * @return connectivity
    */
  def isConnectedComplex: Boolean = {
    val vOpt = twoComplex.vertices.toList.headOption
    vOpt.map{v => connectedComponent(v) == twoComplex.vertices}.getOrElse(true)
  }

  /** 
   *given an edge, find a face whose boundary contains e (if it exists, it is unique); 
   * take the next edge along the boundary
   */
  def succOpt (e : Edge) : Option[Edge] = {
      val mayBefaceOfEdge = twoComplex.faces.find(_.boundary.contains(e))
      mayBefaceOfEdge flatMap {
        faceOfEdge => 
          val indexOfEdge = faceOfEdge.boundary.indexOf(e)
          if (indexOfEdge <= -1) None
          else if (indexOfEdge == faceOfEdge.boundary.length - 1) Some(faceOfEdge.boundary.head)
          else Some(faceOfEdge.boundary(indexOfEdge + 1))
      }
  }    
  /** 
   *given an edge, find a face whose boundary contains e (if it exists, it is unique); 
   * take the previous edge along the boundary 
   */
  def predOpt (e : Edge) : Option[Edge] = {
      val mayBefaceOfEdge = twoComplex.faces.find(_.boundary.contains(e))
      mayBefaceOfEdge flatMap {
        faceOfEdge => 
          val indexOfEdge = faceOfEdge.boundary.indexOf(e)
          if (indexOfEdge <= -1) None
          else if (indexOfEdge == 0) Some(faceOfEdge.boundary.last)
          else Some(faceOfEdge.boundary(indexOfEdge - 1))
      }
  }        

  /** 
   *gives the edge with same terminal vertex obtained by left rotation.
   */
  def rotateLeftOpt (e : Edge) : Option[Edge] = {
    succOpt(e) flatMap {
      f => Some(f.flip)
    }
  }

  /** 
   *gives the edge with same terminal vertex obtained by right rotation.
   */
  def rotateRightOpt (e : Edge) : Option[Edge] = predOpt(e.flip)
  
  /** 
   *auxilliary function to start with an edge and take all edges by rotating left 
   */
  def orbit (e : Edge, steps : Int, opt  : Edge => Option[Edge], accum : Set[Edge]) : Set[Edge] = {
    if (steps <= 0) accum
    else { 
     val nextEdge = opt(e)
     nextEdge.fold(accum + e)(f => orbit(f, steps - 1, opt, accum + e)  )
    }
  } 

  /**
   *all edges to the left of the edge e including itself
   */  
  def allEdgesToTheLeftOf (e : Edge) = orbit(e, edges.size + 1, rotateLeftOpt(_), Set.empty)
  
  /** 
   *all edges to the left of the edge e including itself
   */
  def allEdgesToTheRightOf (e : Edge) = orbit(e, edges.size + 1, rotateRightOpt(_), Set.empty)
  
  /** 
   *set of all edges ending at v 
   */
  def edgesEndingAt (v : Vertex) = 
    ((twoComplex.edges.filter(_.terminal == v).toSet) ++ // FIXME the second term is not needed for a valid two-complex
     (twoComplex.edges.filter(_.initial == v).map(_.flip)))

  /**
   * The degree of a vertex
   */
  def degree(v: Vertex): Int = edgesEndingAt(v).size

  /** 
   * checks if we start with an edge e with v == e.terminal, using left rotations, 
   * (by iterating) we should get all edges with terminal vertex v.
   * The naming is slightly misleading. Do give suggestions for better names
  */
  def transitiveRotations (v : Vertex) : Boolean = {
    assert( twoComplex.vertices.contains(v), "vertex is not part of the complex")
    val edgesEndingAtVertex = edgesEndingAt(v) // set of all edges ending at v
  
    if (edgesEndingAtVertex.nonEmpty) {
      ((edgesEndingAtVertex == allEdgesToTheLeftOf(edgesEndingAtVertex.head)) 
       && 
      (edgesEndingAtVertex == allEdgesToTheRightOf(edgesEndingAtVertex.head))) 
    }
    else true // if there are no edges ending at v then there is nothing to check
  }   
  
  /**
   * Occurences of edges in faces, counting multiplicity
   *
   * @param e
   */
  def edgeOccurences(e: Edge) : Int = faces.flatMap(_.boundary).count(_ == e)

  /**
   * Checks if the given edge is at the boundary. That is exactly one of
   * e and e.flip is inside a face of the twocomplex
   */
  def isEdgeAtBoundary (e : Edge) : Boolean = {
    ( (edgeOccurences(e) == 0) && (edgeOccurences(e.flip) >= 1)
    ||(edgeOccurences(e) >= 1) && (edgeOccurences(e.flip) == 0))
  }

  /*
   * Checks if the twoComplex is a closed surface. 
   */
  def isClosedSurface : Boolean = {
    
    // checks if the edge e is in exactly one face
    def edgeInOneFace (e : Edge): Boolean = {
     edgeOccurences(e) == 1
    } 
    
    // checks if each edge is in exactly one face
    val condition1 = twoComplex.edges.forall(edgeInOneFace(_)) 
    // every vertex is in some edge
    val condition2 = twoComplex.edges.flatMap(ed => Set(ed.initial, ed.terminal)) == twoComplex.vertices
    // for all veritces one can get all edges ending at it by going around by either left or right turns (not both) 
    val condition3 = vertices.toList.foldLeft(true)(_ && twoComplex.transitiveRotations(_))

    condition1 && condition2 && condition3
  }

  /** 
   *Checks if the twoComplex is a surface with boundary
   */
  def isSurfaceWithBoundary : Boolean = {

    // checks if the edge e is in at least m and at most n faces
    def checkEdge (e : Edge, m : Int, n : Int) : Boolean = {
      val facesContaniningEdge = faces.filter(_.boundary.contains(e))
      ((facesContaniningEdge.size >= m) && (facesContaniningEdge.size <= n))
    }

    // checks if each edge is in at most one face
    val condition1 = twoComplex.edges.forall(e => edgeOccurences(e) <= 1)

    // checks if for each edge e, at least e or e.flip is in one of the faces
    val condition2 = twoComplex.edges.forall(e => edgeOccurences(e) + edgeOccurences(e.flip) >= 1)

    // checks if for the vertex v, one can get all ending at it by going around by left and right turns
    def check (v : Vertex) : Boolean = {
      val edgesEndingAtVertex = edgesEndingAt(v) // edges around v
      if (edgesEndingAtVertex.nonEmpty) {
        val picked = edgesEndingAtVertex.head // pick an edge
        // take all edges by left and right turns
        val allAroundPicked = (allEdgesToTheLeftOf(picked) ++ allEdgesToTheRightOf(picked)) 
        // check if they cover all edges around v and the picked edge is part of a face
        (allAroundPicked == edgesEndingAtVertex)       
       }
      else true // if there are no edges ending at v then there is nothing to check 
    } 
    // check this for all vertices
    val condition3 = vertices.toList.foldLeft(true)(_ && check(_))
    
    condition1 && condition2 && condition3
  }  

  /** 
   *Given a set of vertices vs gives the TwoComplex got by adding vs 
   *to the existing twoComplex. If vs is already inside gives the same 
   *twoComplex 
   */
  def addVertices (vs : Set[Vertex]) : TwoComplex ={
    if (twoComplex.vertices.intersect(vs).nonEmpty) {
      System.err.println("[Warning] The following vertices already belong to the twocomplex" 
        + twoComplex + "\n" + twoComplex.vertices.intersect(vs))
    }

    object newComplex extends TwoComplex {
      val faces: Set[Polygon] = twoComplex.faces
      val edges: Set[Edge] = twoComplex.edges
      val vertices: Set[Vertex] = twoComplex.vertices ++ vs
    }
    newComplex
  }
  
  /** 
   *Given a set of edges eds gives the TwoComplex got by adding eds 
   *and there flips to the existing twoComplex.
   */
  def addEdges (eds : Set[Edge]) : TwoComplex ={
    if (twoComplex.edges.intersect(eds).nonEmpty) {
      System.err.println("[Warning] The following edges already belong to the twocomplex" 
        + twoComplex + "\n" + twoComplex.edges.intersect(eds))
    }

    object newComplex extends TwoComplex {
      val faces: Set[Polygon] = twoComplex.faces
      val edges: Set[Edge] = twoComplex.edges ++ eds ++ eds.map(_.flip)
      val vertices: Set[Vertex] = 
        twoComplex.vertices ++ eds.flatMap(ed => Set(ed.initial, ed.terminal))
    }
    newComplex
  }

  /** 
   *Given a set of faces fcs gives the TwoComplex got by adding fcs 
   *to the existing twoComplex.
   */
  def addfaces (fcs : Set[Polygon]) : TwoComplex = {
    if (twoComplex.faces.intersect(fcs).nonEmpty) {
      System.err.println("[Warning] The following edges already belong to the twocomplex" 
        + twoComplex + "\n" + twoComplex.faces.intersect(fcs))
    }

    object newComplex extends TwoComplex {
      val faces: Set[Polygon] = twoComplex.faces ++ fcs
      val edges: Set[Edge] = twoComplex.edges ++ fcs.flatMap(_.edges)
      val vertices: Set[Vertex] = 
        twoComplex.vertices ++ fcs.flatMap(_.vertices)
    }
    newComplex
  }

  /**
   *Gives the result of adding the given set of twocomplexes to the existing one.
   */
  def addTwoComplexes (complexes : Set[TwoComplex]) = {
    object newComplex extends TwoComplex {
      val faces: Set[Polygon] = twoComplex.faces ++ complexes.flatMap(_.faces)
      val edges: Set[Edge] = twoComplex.edges ++ complexes.flatMap(_.edges)
      val vertices: Set[Vertex] = 
        twoComplex.vertices ++ complexes.flatMap(_.vertices)
    }
    newComplex
  }

  /**
   *Given a set of vertices gives the subcomplex on the vertices
   */
  def subComplex (vs : Set[Vertex]) : TwoComplex = {
    if (!vs.subsetOf(twoComplex.vertices)) {
      System.err.println("[Warning] The following vertices don't belong to the twoComplex : " + "\n " +
        vs.filter(!twoComplex.vertices.contains(_)))
    } 

    object newComplex extends TwoComplex {
      val faces: Set[Polygon] = twoComplex.faces.filter(_.vertices.subsetOf(vs))
      val edges: Set[Edge] = twoComplex.edges.filter(ed => (Set(ed.initial, ed.terminal).subsetOf(vs)))
      val vertices: Set[Vertex] = vs
    }
    newComplex
  }


  /**
    * Turns left in an EdgePath
    *
    * @param e
    * @return
    */
  def turnLeft(e: Edge): Option[Edge] = succOpt(e)

  /**
   * Turns right in an EdgePath
   */
  def turnRight(e: Edge): Option[Edge] = rotateRightOpt(e).flatMap(x => Some(x.flip))
  /**
   *Given e rotates left twice and flips it. This is same as rotating left once and then taking the successor. 
   */
  def slightLeft (e : Edge) : Option[Edge] = rotateLeftOpt(e).flatMap(succOpt) 
  /**
   *Given e takes two right rotations and flips it.
   */
  def slightRight (e : Edge) : Option[Edge] = rotateRightOpt(e).flatMap(rotateRightOpt).map(_.flip)

  /**
   * Forced version of turnLeft, for geodesics and edgepaths
   */
  def L(e: Edge): Edge = {
    require(turnLeft(e) != None , s"No left turn from edge $e")
    turnLeft(e).get
  }
  /**
   * Forced version of turnRight, for geodesics and edgepaths
   */
  def R(e: Edge): Edge = {
    require(turnRight(e) != None, s"No right turn from edge $e")
    turnRight(e).get
  }

  /**
   * Forced version of slightLeft, for geodesics and edgepaths
   */
  def SL(e: Edge): Edge = {
    require(slightLeft(e) != None, s"No slight left from edge $e")
    slightLeft(e).get
  }

  /**
   * Forced version of slightRight, for geodesics and edgepaths
   */
  def SR(e: Edge): Edge = {
    require(slightRight(e) != None, s"No slight right from edge $e")
    slightRight(e).get
  }

  /**
   * Forced swivelLeft, for geodesics and edgepaths
   */
  def SwL(e: Edge): Edge = {
    require(turnRight(e.flip) != None, s"Can't swivel left from edge $e")
    turnRight(e.flip).get
  }

  /**
   * Forced swivelRight, for geodesics and edgepaths
   */
  def SwR(e: Edge): Edge = {
    require(turnLeft(e.flip) != None, s"Can't swivel right from edge $e")
    turnLeft(e.flip).get
  }

  /**
    * Vector of edges to the left of an edge, modified version of orbit
    */
    def vectorOrbit (e : Edge, opt: (Edge => Option[Edge]), accum : Vector[Edge]) : Vector[Edge] = {
        val nextEdge = opt(e)
        if ((nextEdge != None) && (! accum.contains(nextEdge))) vectorOrbit(nextEdge.get, opt, accum :+ nextEdge.get)
        else accum
      }
  
    /**
     * Vector of flips of edges to the left of an edge
     */

     def vectorLeftTurns(e: Edge) = vectorOrbit(e, rotateLeftOpt(_), Vector[Edge](e)).map(_.flip)

     /**
       * Vector of flips of edges to the right of an edge
       */

    def vectorRightTurns(e: Edge) = vectorOrbit(e, rotateRightOpt(_), Vector[Edge](e)).map(_.flip)

// --------------------------------------------------------------------------------------------------------------------
  
  /**
  * Vector of edges to the left of an edge
  */

  def vectorEdgesToTheLeftOf(e: Edge) = vectorOrbit(e, rotateLeftOpt(_), Vector[Edge]())

  /**
  * Vector of edges to the right of an edge
  */

  def vectorEdgesToTheRightOf(e: Edge) = vectorOrbit(e, rotateRightOpt(_), Vector[Edge]())    
}


/**
  * A two-complex with all vertices and edges contained in faces, hence determined by its faces.
  */
trait PureTwoComplex extends TwoComplex {
  val faces: Set[Polygon]

  lazy val edges: Set[Edge] =
    faces.map(_.edges).foldLeft(Set.empty[Edge])(_ union _)

  lazy val vertices: Set[Vertex] =
    faces.map(_.vertices).foldLeft(Set.empty[Vertex])(_ union _)
}

