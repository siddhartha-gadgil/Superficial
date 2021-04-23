package superficial

import Polygon.Index

/**
  * A normal arc in a face
  * @param initial the edge containing the initial point
  * @param terminal the edge containing the final point
  * @param face the face containing the arc
  */
case class NormalArc[P <: Polygon](initial: Index, terminal: Index, face: P) {
  val terminalEdge = face.boundary(terminal)

  val initialEdge = face.boundary(initial)

  lazy val flip = NormalArc[P](terminal, initial, face)

  def vertexLinking = math.abs((terminal - initial) % (face.sides)) == 1

  def whichVertexLinking: Option[Vertex] =
    (terminal - initial) % face.sides match {
      case 1  => Some(face.boundary(initial).terminal)
      case -1 => Some(face.boundary(terminal).terminal)
      case _  => None
    }

  def crosses(that: NormalArc[P]) =
    (that.initial - initial) * (that.terminal - terminal) * (that.initial - terminal) * (that.terminal - initial) < 0

}

object NormalArc {
  def enumerate[P <: Polygon](complex: TwoComplex[P]): Set[NormalArc[P]] =
    for {
      face <- complex.faces
      initial <- face.indices
      terminal <- face.indices
      if terminal != initial
    } yield NormalArc(initial, terminal, face)
}

case class NormalPath[P <: Polygon](edges: Vector[NormalArc[P]]) {
  edges.zip(edges.tail).foreach {
    case (e1, e2) =>
      require(
        e1.terminalEdge == e2.initialEdge || e1.terminalEdge == e2.initialEdge.flip,
        s"terminal point on ${e1.terminalEdge} of $e1 is not initial point of $e2 on ${e2.initialEdge}"
      )
      require(!(e1.terminalEdge.isInstanceOf[BoundaryEdge]))
  }

  val length = edges.size

  lazy val flip = NormalPath[P](edges.reverse.map(_.flip))

  def +:(arc: NormalArc[P]) = NormalPath(arc +: edges)

  def :+(arc: NormalArc[P]) = NormalPath(edges :+ arc)
  //
  //  def appendOpt(arc: NormalArc): Option[NormalPath] =
  //    if (arc.initial == terminalEdge && arc != edges.last.flip) Some(this :+ arc)
  //    else None

  val isClosed
      : Boolean = (edges.last.terminalEdge == edges.head.initialEdge) || (edges.last.terminalEdge == edges.head.initialEdge.flip)

  //  val initEdge: Edge = edges.head.initial
  //
  val (terminalFace, terminalIndex) = edges.last.face -> edges.last.terminal

  val terminalEdge =
    terminalFace.boundary(terminalIndex)

  val (initialFace, initialIndex) = edges.head.face -> edges.head.initial

  val initialEdge =
    initialFace.boundary(initialIndex)

  def distinctFaces: Boolean = edges.map(_.face).distinct.size == edges.size

  def linkingPair = edges.zip(edges.tail :+ edges.head).exists {
    case (x, y) => x.vertexLinking && y.vertexLinking
  }

  def distinctEdges = {
    val indEdg = edges.zipWithIndex
    for {
      (x, i) <- indEdg
      (y, j) <- indEdg
    } yield (x, y)
  }

  def withCross = distinctEdges.exists { case (x, y) => x.crosses(y) }

  def geodesicCandidate = !(linkingPair || withCross)
}

object NormalPath {

  @annotation.tailrec
  def enumerateRec[P <: Polygon](
      complex: TwoComplex[P],
      maxAppendLength: Option[Int],
      p: NormalPath[P] => Boolean,
      latest: Set[NormalPath[P]],
      accum: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    if (maxAppendLength.contains(0) || latest.isEmpty) accum
    else {
      val newPaths =
        (
          for {
            path <- latest
            (face, i1) <- complex.edgeIndices(path.terminalEdge).map {
              case (f, i, _) => (f, i)
            } -
              (path.terminalFace -> path.terminalIndex)
            i2 <- face.indices
            if i2 != i1
            arc = NormalArc(i1, i2, face)
          } yield path :+ arc
        ).filter(p)
      enumerateRec(
        complex,
        maxAppendLength.map(_ - 1),
        p,
        newPaths.filter(path => !(endsGoAround(complex, path))),
        accum union newPaths
      )
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
  def enumerate[P <: Polygon](
      complex: TwoComplex[P],
      maxLength: Option[Int] = None,
      p: NormalPath[P] => Boolean = (p: NormalPath[P]) => true
  ): Set[NormalPath[P]] =
    if (maxLength.exists(_ < 1)) Set()
    else {
      val lengthOne =
        NormalArc
          .enumerate(complex)
          .map((arc) => NormalPath(Vector(arc)))
          .filter(p)
      enumerateRec(complex, maxLength.map(_ - 1), p, lengthOne, lengthOne)
    }

  def startEndSameFace[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P]
  ): Boolean =
    complex
      .edgeIndices(path.edges.head.initialEdge)
      .filter {
        case (f, i, _) =>
          !((f == path.edges.head.face) && (i == path.edges.head.initial))
      }
      .map {
        case (f, _, _) => f
      }
      .head == complex
      .edgeIndices(path.edges.last.terminalEdge)
      .filter {
        case (f, i, _) =>
          !((f == path.edges.last.face) && (i == path.edges.last.terminal))
      }
      .map {
        case (f, _, _) => f
      }
      .head

  def endsGoAround[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P]
  ): Boolean =
    endsGoAroundrec(
      complex,
      path.edges.init,
      path.edges.last.whichVertexLinking,
      NormalPath[P](Vector(path.edges.last))
    )

  def endsGoAroundrec[P <: Polygon](
      complex: TwoComplex[P],
      initedges: Vector[NormalArc[P]],
      optvertex: Option[Vertex],
      accum: NormalPath[P]
  ): Boolean = optvertex match {
    case None => false
    case Some(v) =>
      initedges.isEmpty match {
        case true => startEndSameFace(complex, accum)
        case false =>
          initedges.last.whichVertexLinking match {
            case Some(newv) =>
              if (newv == v)
                endsGoAroundrec(
                  complex,
                  initedges.init,
                  optvertex,
                  NormalPath[P](initedges.last +: accum.edges)
                )
              else startEndSameFace(complex, accum)
            case None => startEndSameFace(complex, accum)
          }
      }
  }

  def removeFlipAndCyclicPerRec[P <: Polygon](
      accum: Set[NormalPath[P]],
      paths: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    if (paths.isEmpty) accum
    else {
      removeFlipAndCyclicPerRec(
        accum + paths.head,
        paths.filter(
          p =>
            (p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet)
        )
      )
    }
  }

  def removeFlipAndCyclicPer[P <: Polygon](
      paths: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    require(paths.forall(_.isClosed))
    if (paths.isEmpty) paths
    else {
      removeFlipAndCyclicPerRec(
        Set(paths.head),
        paths.filter(
          p =>
            (p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet)
        )
      )
    }
  }
}
