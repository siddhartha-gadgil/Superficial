package superficial

import Polygon.Index

/**
  * Abstract polygon, with given edges and vertices, i.e. a two-complex with a single face.
  * @param sides number of sides
  */
trait Polygon extends TwoComplex {
  val sides: Int

  lazy val faces = Set(this)

  lazy val indices: Vector[Index] = (0 until sides).toVector

  val boundary: Vector[Edge]

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

class EdgePair(initial: Vertex, terminal: Vertex){pair =>
  case object Positive extends Edge{
    val initial = pair.initial
    val terminal = pair.terminal

    lazy val flip: Edge = Negative
  }

  case object  Negative extends Edge{
    val initial = pair.terminal
    val terminal = pair.initial

    lazy val flip: Edge = Positive
  }
}

/**
  * An oriented edge in a two-complex
  */
trait Edge {

  /**
    * the same edge with the opposite orientation.
    */
  def flip: Edge

  def terminal: Vertex

  def initial: Vertex

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
    def flip: Edge = Symbolic(name, terminal, initial, !positivelyOriented)
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
  case class Impl(vertices: Set[Vertex], edges: Set[Edge], faces: Set[Polygon]) extends TwoComplex

  def symbolic(
      vertexNames: String*)(
      edgeMap: (String, (String, String))*)(
      faceMap: (String, Vector[(String, Boolean)])*
  ): TwoComplex =
    {
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
trait TwoComplex {
  def faces: Set[Polygon]

  def edges: Set[Edge] // these come in pairs, related by flip (reversing orientation)

  lazy val positiveEdges =
    edges.toVector.collect {
      case oe: OrientedEdge if oe.positivelyOriented => oe
    }

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
