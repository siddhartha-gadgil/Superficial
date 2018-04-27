package superficial

/**
  * Abstract polygon, with given edges and vertices
  * @param sides number of sides
  */
abstract class Polygon(val sides: Int) extends TwoComplex {
  lazy val faces = Set(this)

  val edges: Set[Edge]

  val vertices: Set[Vertex]
}

object Polygon {

  /**
    * Construction of a polygon given number of sides
    * @param n number of sides
    * @return a polgon with n sides
    */
  def apply(n: Int): Polygon = new Polygon(n) { self =>
    lazy val edges: Set[Edge] =
      (for (e <- 0 until sides; ori <- Set(true, false))
        yield PolygonEdge(self, e, ori)).toSet

    lazy val vertices: Set[Vertex] =
      ((0 until sides) map (PolygonVertex(self, _))).toSet
  }

  case class PolygonEdge(polygon: Polygon,
                         index: Int,
                         positiveOriented: Boolean)
      extends Edge {
    lazy val flip: PolygonEdge = PolygonEdge(polygon, index, !positiveOriented)

    lazy val head: PolygonVertex =
      if (positiveOriented)
        PolygonVertex(polygon, (index + 1) % polygon.sides)
      else
        PolygonVertex(polygon, index)

    lazy val tail: PolygonVertex =
      if (!positiveOriented)
        PolygonVertex(polygon, (index + 1) % polygon.sides)
      else
        PolygonVertex(polygon, index)

  }

  case class PolygonVertex(polygon: Polygon, index: Int) extends Vertex

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

  def head: Vertex

  def tail: Vertex
}

case class QuotientEdge(edges: Set[Edge]) extends Edge {
  def flip = QuotientEdge(edges map (_.flip))

  def head = QuotientVertex(edges map (_.head))

  def tail = QuotientVertex(edges map (_.tail))

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

  def normalArcs: Set[NormalArc] =
    for {
      face <- faces
      initial <- face.edges
      terminal <- face.edges
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
case class NormalArc(initial: Edge, terminal: Edge, face: Polygon) {
  lazy val flip = NormalArc(initial.flip, terminal.flip, face)

  require(face.edges.contains(initial) && face.edges.contains(terminal),
          s"the face $face should contain edges $initial and $terminal")
}

object NormalArc{
  def enumerate(complex: TwoComplex): Set[NormalArc] =
    for {
      face <- complex.faces
      initial <- face.edges
      terminal <- face.edges - initial
    } yield NormalArc(initial, terminal, face)
}

case class NormalPath(edges: Vector[NormalArc]) {
  edges.zip(edges.tail).foreach {
    case (e1, e2) =>
      require(e1.terminal == e2.initial,
              s"terminal point of $e1 is not initial point of $e2")
  }

  def +:(arc: NormalArc) = NormalPath(arc +: edges)

  def :+(arc: NormalArc) = NormalPath(edges :+ arc)

  def appendOpt(arc: NormalArc): Option[NormalPath] =
    if (arc.initial == terminalEdge && arc != edges.last.flip) Some(this :+ arc) else None

  val isClosed: Boolean = edges.last.terminal == edges.head.initial

  val initEdge: Edge = edges.head.initial

  val terminalEdge: Edge = edges.last.terminal

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
            arc <- complex.normalArcs
          } yield path.appendOpt(arc)
        ).flatten.filter(p)
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
      NormalArc.enumerate(complex).map((arc) => NormalPath(Vector(arc))).filter(p)
    enumerateRec(complex, maxLength.map(_ - 1), p,
      lengthOne, lengthOne)
  }
}
