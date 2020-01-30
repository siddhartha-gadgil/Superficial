package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

/**
  * Abstract polygon, with given edges and vertices, i.e. a two-complex with a single face.
  * @param sides number of sides
  */
trait Polygon extends TwoComplex {
  val sides: Int

  lazy val faces = Set(this)

  lazy val indices: Vector[Index] = (0 until sides).toVector

  val boundary: Vector[Edge]

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

  def apply(v: Vector[Edge]): Polygon = {
    assert(checkBoundary(v), s"boundary $v not a loop")
    new Polygon {
      val sides: Int = v.size
      val boundary: Vector[Edge] = v
      val vertices: Set[Vertex] = v.map(_.initial).toSet
    }
    
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
  case class Impl(vertices: Set[Vertex], edges: Set[Edge], faces: Set[Polygon])
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
        allCollapsed(complex.collapseEdge(e))
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
    Polygon(firstHead ++ secondTail ++ secondHead ++ firstTail)
  }.ensuring(poly => poly.checkPoly)

  def symbolic(vertexNames: String*)(edgeMap: (String, (String, String))*)(
      faceMap: (String, Vector[(String, Boolean)])*
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
          Polygon.Symbolic(face, edges)
      }.toSet
    Impl(vertices, edges, faces)
  }
}

/**
  *  A polyheadral two complex, with faces polygons, a collection of edges and
  */
trait TwoComplex { twoComplex =>
  def faces: Set[Polygon]

  def edges: Set[Edge] // these come in pairs, related by flip (reversing orientation)

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
  

  def edgeIndex(edge: Edge) = {
    positiveEdges.zipWithIndex
      .find { case (e, i) => e == edge || e.flip == edge }
      .map { case (e, i) => (i, e == edge) }
  }

  def vertices: Set[Vertex]

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

  def collapseEdge(e: Edge): TwoComplex = {
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

    def newPoly(polygon: Polygon): Polygon =
      new Polygon {
        val sides: Int = polygon.sides
        val boundary: Vector[Edge] =
          polygon.boundary.filterNot(Set(e, e.flip).contains(_)).map { edge =>
            newEdgeMap(edge)
          }
        val vertices: Set[Vertex] = polygon.vertices - e.terminal
        assert(checkBoundary)
      }

    object newComplex extends TwoComplex {
      def edges: Set[Edge] = newEdgeMap.values.toSet
      def faces: Set[Polygon] = twoComplex.faces.map(newPoly(_))
      def vertices: Set[Vertex] = twoComplex.vertices - e.terminal
      override def toString(): String = s"$twoComplex/$e @ $hashCode"
    }
    newComplex
  }

  //Finds neighbours of a vertex
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

  //Finds the connected component of a vertex
  def connectedComponent(v: Vertex): Set[Vertex] = {
    maxSetNbr(Set(v))
  }

  //Checks if the complex is connected
  def isConnectedComplex: Boolean = {
    val v = twoComplex.vertices.toList.head
    connectedComponent(v) == twoComplex.vertices
  }

  // given an edge, find a face whose boundary contains e (if it exists, it is unique); 
  //take the next edge along the boundary
  def succOpt (e : Edge) : Option[Edge] = {
      val mayBefaceOf_e = twoComplex.faces.find(_.boundary.contains(e))
      mayBefaceOf_e flatMap {
        faceOf_e => 
          val indexOf_e = faceOf_e.boundary.indexOf(e)
          if (indexOf_e <= -1) None
          else if (indexOf_e == faceOf_e.boundary.length) Some(faceOf_e.boundary.head)
          else Some(faceOf_e.boundary(indexOf_e + 1))
      }
  }    
  // given an edge, find a face whose boundary contains e (if it exists, it is unique); 
  //take the previous edge along the boundary
  def predOpt (e : Edge) : Option[Edge] = {
      val mayBefaceOf_e = twoComplex.faces.find(_.boundary.contains(e))
      mayBefaceOf_e flatMap {
        faceOf_e => 
          val indexOf_e = faceOf_e.boundary.indexOf(e)
          if (indexOf_e <= -1) None
          else if (indexOf_e == 0) Some(faceOf_e.boundary.last)
          else Some(faceOf_e.boundary(indexOf_e - 1))
      }
  }        

  // gives the edge with same terminal vertex obtained by left rotation.
  def rotateLeftOpt (e : Edge) : Option[Edge] = {
    succOpt(e) flatMap {
      f => Some(f.flip)
    }
  }

  // gives the edge with same terminal vertex obtained by right rotation.
  def rotateRightOpt (e : Edge) : Option[Edge] = predOpt(e.flip)

  // checks if we start with an edge e with v == e.terminal, using left rotations, 
  // (by iterating) we should get all edges with terminal vertex v.
  // The naming is slightly misleading. Do give suggestions for better names

  def isSurroundedVertex (v : Vertex) : Boolean = {
    assert( twoComplex.vertices.contains(v), "vertex is not part of the complex")
    val edgesEndingAt_v = twoComplex.edges.filter(_.terminal == v).toSet // set of all edges ending at v
    
    // auxilliary function to start with an edge and take all edges by rotating left
    def takeSum (e : Edge) (steps : Int) (accum : Set[Edge]) : Set[Edge] = {
      if (steps <= 0) accum
      else { 
        val nextEdge = twoComplex.rotateLeftOpt(e)
        nextEdge match {
          case Some(f) => takeSum(f)(steps - 1)(accum + f)
          case None => accum     
        }
      }
    }

    if (edgesEndingAt_v.nonEmpty) {
      // take all edges by going to the left
      val allEdgesToTheLeft = takeSum(edgesEndingAt_v.head)(edgesEndingAt_v.size)(Set.empty)
      // check if that is same as the set of all edges ending at v
      edgesEndingAt_v == allEdgesToTheLeft 
    }
    else true // if there are no edges ending at v then there is nothing to check
    
  }        
    
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

