package superficial

import Polygon.Index

/**
  * A normal arc in a face
  * @param initial the edge containing the initial point
  * @param terminal the edge containing the final point
  * @param face the face containing the arc
  */
  case class NormalArc(initial: Index, terminal: Index, face: Polygon) {
    val terminalEdge = face.boundary(terminal)
  
    val initialEdge = face.boundary(initial)
  
    def vertexLinking = math.abs(terminal - initial) == 1
  
    def crosses(that: NormalArc) = (that.initial - initial) * (that.terminal - terminal) * (that.initial - terminal) * (that.terminal - initial) < 0
  
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
        require(e1.terminalEdge == e2.initialEdge || e1.terminalEdge == e2.initialEdge.flip,
                s"terminal point on ${e1.terminalEdge} of $e1 is not initial point of $e2 on ${e2.initialEdge}")
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
  
    def linkingPair = edges.zip(edges.tail :+ edges.head).exists{case (x, y) => x.vertexLinking && y.vertexLinking}
  
    def distinctEdges = {
      val indEdg = edges.zipWithIndex
      for {
        (x, i) <- indEdg
        (y, j) <- indEdg
      } yield (x, y)
    }
  
    def withCross = distinctEdges.exists{case (x, y) => x.crosses(y)}
  
    def geodesicCandidate = !(linkingPair || withCross)
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
              (face, i1) <- complex.edgeIndices(path.terminalEdge).map{case (f, i, _) => (f, i)} -
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
  