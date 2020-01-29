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

    // given a polygon P and an edge e gives the next edge of e if e is in P
    def succOpt (e : Edge) : Option[Edge] = {
      val indexOf_e = boundary.indexOf(e);
      if (indexOf_e <= - 1) { None }
      else if (indexOf_e >= boundary.length - 1 ) {Some(boundary.head)}
      else { Some(boundary(indexOf_e + 1)) }
    }   

    // given a polygon P and an edge e gives the previous edge of e if e is in P
    def predOpt (e : Edge) : Option[Edge] = {
      val indexOf_e = boundary.indexOf(e);
      if (indexOf_e <= - 1) { None }
      else if (indexOf_e == 0) {Some(boundary.last)}
      else { Some(boundary(indexOf_e - 1)) }
    } 
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

/**
  * A vertex in a two-complex
  */
class Vertex

object Vertex {
  case class Symbolic(name: String) extends Vertex
}

class EdgePair(initial: Vertex, terminal: Vertex) { pair =>
  case object Positive extends OrientedEdge {
    val initial = pair.initial
    val terminal = pair.terminal

    val positivelyOriented: Boolean = true

    lazy val flip: OrientedEdge = Negative

    override def toString = s"$pair.positive"
  }

  case object Negative extends OrientedEdge {
    val initial = pair.terminal
    val terminal = pair.initial

    val positivelyOriented: Boolean = false

    lazy val flip: OrientedEdge = Positive

    override def toString = s"$pair.negative"
  }
}

/**
  * An oriented edge in a two-complex
  */
trait Edge {

  /**
    * the same (undirected) edge with the opposite orientation.
    */
  def flip: Edge

  def terminal: Vertex

  def initial: Vertex

  def checkFlip: Boolean =
    (flip.terminal == initial) && (flip.initial == terminal) && 
    (flip.flip == this) && (flip != this)

  def del: FormalSum[Vertex] =
    FormalSum.reduced(Vector(terminal -> 1, initial -> -1))
}

object Edge {
  case class Symbolic(
      name: String,
      initial: Vertex,
      terminal: Vertex,
      positivelyOriented: Boolean = true
  ) extends OrientedEdge {
    def flip: OrientedEdge =
      Symbolic(name, terminal, initial, !positivelyOriented)
  }

  def symbolic(
      name: String,
      initialName: String,
      terminalName: String,
      positivelyOriented: Boolean = true
  ) =
    Symbolic(
      name,
      Vertex.Symbolic(initialName),
      Vertex.Symbolic(terminalName),
      positivelyOriented
    )
}

trait OrientedEdge extends Edge {
  val positivelyOriented: Boolean

  def flip: OrientedEdge
}

/**
  * An edge obtained by identifications
  */
case class QuotientEdge(edges: Set[Edge]) extends Edge {
  def flip = QuotientEdge(edges map (_.flip))

  def terminal = QuotientVertex(edges map (_.terminal))

  def initial = QuotientVertex(edges map (_.initial))

}

/**
  * A vertex obtained by identifications
  */
case class QuotientVertex(vertices: Set[Vertex]) extends Vertex

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
  def VertexNbr(v: Vertex): Set[Vertex] = {
    val s = (twoComplex.edges.filter(_.initial == v).map(_.terminal)).union(twoComplex.edges.filter(_.terminal == v).map(_.initial))
    s.union(Set(v))
  }

  //Collects a first order neighbourhood of a set onto a set
  def SetNbr(s: Set[Vertex]): Set[Vertex] = {
    //Accumulates first order neighbours onto a list
    def ListNbr(s: List[Vertex], accum: List[Vertex]): List[Vertex] = {
      s match {
        case x::rest => ListNbr(rest, accum ++ VertexNbr(x))
        case Nil => accum
      }
    
    }
    ListNbr(s.toList, List[Vertex]()).toSet
  }

  //Finds the maximal set of neighbours of a given set
  def MaxSetNbr(s: Set[Vertex]): Set[Vertex] = {
    if (SetNbr(s) == s) s
    else MaxSetNbr(SetNbr(s))
  }

  //Finds the connected component of a vertex
  def ConnectedComponent(v: Vertex): Set[Vertex] = {
    MaxSetNbr(Set(v))
  }

  //Checks if the complex is connected
  def isConnectedComplex: Boolean = {
    val v = twoComplex.vertices.toList.head
    ConnectedComponent(v).equals(twoComplex.vertices)
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

/**
  * A formal sum of elements of `A` with coefficients integers.
  */
case class FormalSum[A](coeffs: Map[A, Int]) {
  val coeffVec = coeffs.toVector

  def ++(that: FormalSum[A]) = FormalSum.reduced(coeffVec ++ that.coeffVec)

  def +(el: A) = FormalSum.reduced(coeffVec :+ (el -> 1))

  def -(el: A) = FormalSum.reduced(coeffVec :+ (el -> -1))

  def map[B](f: A => B): FormalSum[B] =
    FormalSum.reduced(coeffVec.map { case (a, n) => (f(a), n) })

  def flatMap[B](f: A => FormalSum[B]): FormalSum[B] = {
    val cv =
      for {
        (a, n) <- coeffVec
        (b, m) <- f(a).coeffVec
      } yield (b, n * m)
    FormalSum.reduced(cv)
  }
}

object FormalSum {

  /**
    * reduce a formal sum by combining terms and removing zero terms
    */
  def reduced[A](v: Vector[(A, Int)]) = {
    val m = v
      .groupBy(_._1)
      .view
      .mapValues { vc =>
        vc.map(_._2).sum
      }
      .filter(_._2 != 0)
      .toMap
    FormalSum(m)
  }

  /**
    * second boundary map for homology
    */
  def del2(c2: FormalSum[Polygon]) = c2.flatMap(_.del)

  /**
    * first boundary map for homology
    */
  def del1(c1: FormalSum[Edge]) = c1.flatMap(_.del)
}
