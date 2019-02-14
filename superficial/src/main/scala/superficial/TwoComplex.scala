package superficial

import Polygon.Index
/**
  * Abstract polygon, with given edges and vertices
  * @param sides number of sides
  */
abstract class Polygon(val sides: Int) extends TwoComplex {
  lazy val faces = Set(this)
  
  val indices : Vector[Index] = (0 until sides).toVector

  val boundary: Vector[Edge]

  lazy val edges: Set[Edge] =
    boundary.toSet.flatMap((s: Edge) => Set(s, s.flip))

  val vertices: Set[Vertex]

  def boundaryIndex(e: Edge): Set[Int] =
    indices.toSet.filter{
      (n) => e == boundary(n) || e.flip == boundary(n)
    }
}

object Polygon {
  type Index = Int

  /**
    * Construction of a polygon given number of sides
    * @param n number of sides
    * @return a polgon with n sides
    */
  def apply(n: Int): Polygon = new Polygon(n) { self =>
    lazy val boundary: Vector[Edge] =
      for (e <- indices)
        yield PolygonEdge(self, e, positiveOriented = true)

    lazy val vertices: Set[Vertex] =
      (indices map (PolygonVertex(self, _))).toSet
  }

  case class PolygonEdge(polygon: Polygon,
                         index: Index,
                         positiveOriented: Boolean)
      extends Edge {
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
trait Vertex

/**
  * An oriented edge in a two-complex
  */
trait Edge {
  def flip: Edge

  def terminal: Vertex

  def initial: Vertex
}

case class QuotientEdge(edges: Set[Edge]) extends Edge {
  def flip = QuotientEdge(edges map (_.flip))

  def terminal = QuotientVertex(edges map (_.terminal))

  def initial = QuotientVertex(edges map (_.initial))

}

case class QuotientVertex(vertices: Set[Vertex]) extends Vertex

/**
  *  A two complex
  */
trait TwoComplex {
  def faces: Set[Polygon]

  def edges
    : Set[Edge] // these come in pairs, related by flip (reversing orientation)

  def vertices: Set[Vertex]

  def facesWithEdge(edge: Edge): Set[Polygon] =
    faces.filter((face) => face.edges.contains(edge))

  def edgeIndices(edge: Edge): Set[(Polygon, Index)] =
    faces.flatMap((f) =>
      f.boundaryIndex(edge).map((n) => f -> n)
    )

  def normalArcs: Set[NormalArc] =
    for {
      face <- faces
      initial <- face.indices
      terminal <- face.indices
    } yield NormalArc(initial, terminal, face)
}

/**
  * A two-complex with vertices and edges contained in faces.
  */
trait PureTwoComplex extends TwoComplex {
  val faces: Set[Polygon]

  lazy val edges: Set[Edge] =
    faces.map(_.edges).foldLeft(Set.empty[Edge])(_ union _)

  lazy val vertices: Set[Vertex] =
    faces.map(_.vertices).foldLeft(Set.empty[Vertex])(_ union _)
}

/**
  * A normal arc in a face
  * @param initial the edge containing the initial point
  * @param terminal the edge containing the final point
  * @param face the face containing the arc
  */
case class NormalArc(initial: Index, terminal: Index, face: Polygon) {
  val terminalEdge = face.boundary(terminal)

  val initialEdge = face.boundary(initial)

}

object NormalArc {
  def enumerate(complex: TwoComplex): Set[NormalArc] =
    for {
      face <- complex.faces
      initial <- face.indices
      terminal <- face.indices
      if terminal != initial
    } yield NormalArc(initial, terminal, face)
}

case class NormalPath(edges: Vector[NormalArc]) {
  edges.zip(edges.tail).foreach {
    case (e1, e2) =>
      require(e1.terminalEdge == e2.initialEdge,
              s"terminal point of $e1 is not initial point of $e2")
  }

  def +:(arc: NormalArc) = NormalPath(arc +: edges)

  def :+(arc: NormalArc) = NormalPath(edges :+ arc)
//
//  def appendOpt(arc: NormalArc): Option[NormalPath] =
//    if (arc.initial == terminalEdge && arc != edges.last.flip) Some(this :+ arc)
//    else None

  val isClosed: Boolean = edges.last.terminalEdge == edges.head.initialEdge

//  val initEdge: Edge = edges.head.initial
//
  val (terminalFace, terminalIndex) = edges.last.face -> edges.last.terminal

  val terminalEdge =
     terminalFace.boundary(terminalIndex)

  val (initialFace, initialIndex) = edges.head.face -> edges.head.initial

  val initialEdge =
    initialFace.boundary(initialIndex)

  def distinctFaces: Boolean = edges.map(_.face).distinct.size == edges.size
}

object NormalPath {

  @annotation.tailrec
  def enumerateRec(complex: TwoComplex,
                   maxAppendLength: Option[Int],
                   p: NormalPath => Boolean,
                   latest: Set[NormalPath],
                   accum: Set[NormalPath]): Set[NormalPath] = {
    if (maxAppendLength.contains(0) || latest.isEmpty) accum
    else {
      val newPaths =
        (
          for {
            path <- latest
            (face, i1) <- complex.edgeIndices(path.terminalEdge) -
              (path.terminalFace -> path.terminalIndex) -
              (path.edges.last.face -> path.edges.last.initial)
            i2 <- face.indices
            if i2 != i1
            arc = NormalArc(i1, i2, face)
          } yield path :+ arc
        ).filter(p)
      enumerateRec(complex,
                   maxAppendLength.map(_ - 1),
                   p,
                   newPaths,
                   accum union newPaths)
    }
  }

  /**
    * recursively enumerate normal paths satisfying a hereditary condition with
    * optional bound on length;
    * terminates if the length is reached or no new paths were generated in the last step.
    * @param complex  the two-complex
    * @param maxLength an optional maximum length
    * @param p a hereditary condition
    * @return set of patbs with bounded length satisfying the condition
    */
  def enumerate(complex: TwoComplex,
                maxLength: Option[Int] = None,
                p: NormalPath => Boolean = (_) => true): Set[NormalPath] =
    if (maxLength.exists(_ < 1)) Set()
    else {
      val lengthOne =
        NormalArc
          .enumerate(complex)
          .map((arc) => NormalPath(Vector(arc)))
          .filter(p)
      enumerateRec(complex, maxLength.map(_ - 1), p, lengthOne, lengthOne)
    }
}
